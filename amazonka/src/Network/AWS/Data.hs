-- |
-- Module      : Network.AWS.Data
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Re-exports some of the underlying textual and byte serialisation mechanisms
-- for convenience.
--
-- Many of the AWS identifiers like S3's 'ObjectVersionId' or 'ETag',
-- as well as any nullary sum types such as 'Network.AWS.Region' have 'ToText'
-- and 'ToByteString' instances, making it convenient to use the type classes
-- to convert a value to its textual representation.
module Network.AWS.Data
    (
    -- * Text
      FromText     (..)
    , fromText
    , fromTextError
    , takeLowerText
    , ToText       (..)

    -- * ByteString
    , ToByteString (..)

    -- * Log Messages
    , ToLog        (..)
    ) where

import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Log
import           Network.AWS.Data.Text
