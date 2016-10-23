{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Example.APIGateway where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.Monoid
import Network.AWS.APIGateway
import Network.AWS.Data
import System.IO

main :: Region -> IO Method
main r = do
    lgr <- newLogger Trace stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r
    runResourceT . runAWST env $ do
        restApi <- send (createRestAPI "myApi")
        let Just apiId = restApi ^. raId

        resources :: [Resource] <- (view grrsItems) <$> send (getResources apiId)
        let Just rootId = head resources ^. rId

        resource :: Resource <- send (createResource apiId rootId "{file}")

        let Just fileResourceId = resource ^. rId
        method :: Method <- send (putMethod apiId fileResourceId "GET" "NONE")

        return method
