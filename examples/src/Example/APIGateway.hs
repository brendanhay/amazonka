{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Example.APIGateway where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Monoid
import           Network.AWS.Data
import           Network.AWS.APIGateway
import           System.IO

main :: IO ()
main = do
    lgr <- newLogger Trace stdout
    env <- newEnv Ireland Discover <&> envLogger .~ lgr
    runResourceT (runAWST env go) >>= print

go :: AWST _ _
go = do
    restApi <- send (createRestAPI "myApi")
    let Just apiId = restApi ^. raId
    resources :: [Resource] <- (view grrsItems) <$> send (getResources apiId)
    let Just rootId = head resources ^. rId
    resource :: Resource <- send (createResource apiId rootId "{file}")
    let Just fileResourceId = resource ^. rId
    method :: Method <- send (putMethod apiId fileResourceId "GET" "NONE")
    return method
