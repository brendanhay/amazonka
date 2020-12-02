{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryptionRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ServerSideEncryptionRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ServerSideEncryptionByDefault

-- | Specifies the default server-side encryption configuration.
--
--
--
-- /See:/ 'serverSideEncryptionRule' smart constructor.
newtype ServerSideEncryptionRule = ServerSideEncryptionRule'
  { _sserApplyServerSideEncryptionByDefault ::
      Maybe ServerSideEncryptionByDefault
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerSideEncryptionRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sserApplyServerSideEncryptionByDefault' - Specifies the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied.
serverSideEncryptionRule ::
  ServerSideEncryptionRule
serverSideEncryptionRule =
  ServerSideEncryptionRule'
    { _sserApplyServerSideEncryptionByDefault =
        Nothing
    }

-- | Specifies the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied.
sserApplyServerSideEncryptionByDefault :: Lens' ServerSideEncryptionRule (Maybe ServerSideEncryptionByDefault)
sserApplyServerSideEncryptionByDefault = lens _sserApplyServerSideEncryptionByDefault (\s a -> s {_sserApplyServerSideEncryptionByDefault = a})

instance FromXML ServerSideEncryptionRule where
  parseXML x =
    ServerSideEncryptionRule'
      <$> (x .@? "ApplyServerSideEncryptionByDefault")

instance Hashable ServerSideEncryptionRule

instance NFData ServerSideEncryptionRule

instance ToXML ServerSideEncryptionRule where
  toXML ServerSideEncryptionRule' {..} =
    mconcat
      [ "ApplyServerSideEncryptionByDefault"
          @= _sserApplyServerSideEncryptionByDefault
      ]
