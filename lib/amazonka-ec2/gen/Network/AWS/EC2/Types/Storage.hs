{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Storage where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.S3Storage
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the storage location for an instance store-backed AMI.
--
--
--
-- /See:/ 'storage' smart constructor.
newtype Storage = Storage' {_sS3 :: Maybe S3Storage}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Storage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sS3' - An Amazon S3 storage location.
storage ::
  Storage
storage = Storage' {_sS3 = Nothing}

-- | An Amazon S3 storage location.
sS3 :: Lens' Storage (Maybe S3Storage)
sS3 = lens _sS3 (\s a -> s {_sS3 = a})

instance FromXML Storage where
  parseXML x = Storage' <$> (x .@? "S3")

instance Hashable Storage

instance NFData Storage

instance ToQuery Storage where
  toQuery Storage' {..} = mconcat ["S3" =: _sS3]
