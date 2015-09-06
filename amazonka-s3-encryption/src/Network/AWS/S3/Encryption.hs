{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Network.AWS.S3.Encryption
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption
    (
    -- * Errors

    -- * Operations

    -- ** PutEncryptedObject
    -- , module PutEncryptedObject

    -- ** GetEncryptedObject
    -- , module GetEncryptedObject

    ) where

-- import           Network.AWS.S3.Encryption.GetObject
import           Network.AWS.S3.Encryption.Types
--import           Network.AWS.S3.Encryption.PutObject

-- - Note about or enforce the chunkSize to be a multiple of 128?
-- - How to get this module to appear in the generated cabal file?
-- - How to customise materials, kms vs aes etc.
-- - FUTURE: getObject should be transparent, or, if instruction file is supported then not?

-- target :: AWSRequest a => Lens' (Encryd a) a
-- target = lens _target (\s a -> s { _tar = a })

-- setChunkSize :: MonadResource m => Int -> Conduit ByteString m ByteString
-- setChunkSize n = vectorBuilderC n mapM_CE =$= mapC fromByteVector

-- FIXME: somehow updating an existing files instructions/metadata?

-- initiateMultiPartUpload
-- uploadPart
-- getobject
-- putobject
