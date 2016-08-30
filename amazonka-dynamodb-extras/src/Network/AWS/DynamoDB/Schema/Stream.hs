{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.AWS.DynamoDB.Schema.Stream
    ( StreamSpecification
    , StreamViewType  (..)

    , StreamingDisabled
    , Streaming

    , DynamoStreaming (..)
    ) where

import Control.Lens ((?~))

import Data.Function ((&))
import Data.Proxy    (Proxy (..))

import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.Schema.Types

class DynamoStreaming a where
    -- | Get the 'StreamSpecification' describing a table's streaming configuration.
    getStreaming :: Proxy a -> StreamSpecification

instance DynamoStreaming s => DynamoStreaming (Table n a t s is) where
    getStreaming _ = getStreaming (Proxy :: Proxy s)

instance DynamoStreaming StreamingDisabled where
    getStreaming _ = streaming Nothing

instance DynamoStreaming (Streaming 'SVTKeysOnly) where
    getStreaming _ = streaming (Just SVTKeysOnly)

instance DynamoStreaming (Streaming 'SVTNewAndOldImages) where
    getStreaming _ = streaming (Just SVTNewAndOldImages)

instance DynamoStreaming (Streaming 'SVTNewImage) where
    getStreaming _ = streaming (Just SVTNewImage)

instance DynamoStreaming (Streaming 'SVTOldImage) where
    getStreaming _ = streaming (Just SVTOldImage)

streaming :: Maybe StreamViewType -> StreamSpecification
streaming = \case
    Nothing -> streamSpecification & ssStreamEnabled ?~ False
    Just v  -> streamSpecification & ssStreamEnabled ?~ True
        & ssStreamViewType ?~ v
