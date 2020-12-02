{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.TargetGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.TargetGrant where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.BucketLogsPermission
import Network.AWS.S3.Types.Grantee

-- | Container for granting information.
--
--
--
-- /See:/ 'targetGrant' smart constructor.
data TargetGrant = TargetGrant'
  { _tgPermission ::
      !(Maybe BucketLogsPermission),
    _tgGrantee :: !(Maybe Grantee)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgPermission' - Logging permissions assigned to the grantee for the bucket.
--
-- * 'tgGrantee' - Container for the person being granted permissions.
targetGrant ::
  TargetGrant
targetGrant =
  TargetGrant' {_tgPermission = Nothing, _tgGrantee = Nothing}

-- | Logging permissions assigned to the grantee for the bucket.
tgPermission :: Lens' TargetGrant (Maybe BucketLogsPermission)
tgPermission = lens _tgPermission (\s a -> s {_tgPermission = a})

-- | Container for the person being granted permissions.
tgGrantee :: Lens' TargetGrant (Maybe Grantee)
tgGrantee = lens _tgGrantee (\s a -> s {_tgGrantee = a})

instance FromXML TargetGrant where
  parseXML x =
    TargetGrant' <$> (x .@? "Permission") <*> (x .@? "Grantee")

instance Hashable TargetGrant

instance NFData TargetGrant

instance ToXML TargetGrant where
  toXML TargetGrant' {..} =
    mconcat ["Permission" @= _tgPermission, "Grantee" @= _tgGrantee]
