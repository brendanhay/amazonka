{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.AWS.DynamoDB.Schema.Stream
    ( StreamSpecification
    , StreamViewType  (..)

    , StreamingKind (..)
    , StreamingDisabled
    , Streaming

    , DynamoStreaming (..)
    ) where

import Control.Lens ((?~))

import Data.Function ((&))
import Data.Proxy    (Proxy (..))

import Network.AWS.DynamoDB

-- |
--
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html
data StreamingKind
    = StreamingDisabled
    | Streaming StreamViewType

type StreamingDisabled = 'StreamingDisabled
type Streaming         = 'Streaming

class DynamoStreaming a where
    -- | Get the 'StreamSpecification' describing a table's streaming configuration.
    getStreaming :: Proxy a -> StreamSpecification

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
