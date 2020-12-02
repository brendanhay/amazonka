{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PublicAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PublicAccess where

import Network.AWS.GuardDuty.Types.PermissionConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the public access policies that apply to the S3 bucket.
--
--
--
-- /See:/ 'publicAccess' smart constructor.
data PublicAccess = PublicAccess'
  { _paPermissionConfiguration ::
      !(Maybe PermissionConfiguration),
    _paEffectivePermission :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paPermissionConfiguration' - Contains information about how permissions are configured for the S3 bucket.
--
-- * 'paEffectivePermission' - Describes the effective permission on this bucket after factoring all attached policies.
publicAccess ::
  PublicAccess
publicAccess =
  PublicAccess'
    { _paPermissionConfiguration = Nothing,
      _paEffectivePermission = Nothing
    }

-- | Contains information about how permissions are configured for the S3 bucket.
paPermissionConfiguration :: Lens' PublicAccess (Maybe PermissionConfiguration)
paPermissionConfiguration = lens _paPermissionConfiguration (\s a -> s {_paPermissionConfiguration = a})

-- | Describes the effective permission on this bucket after factoring all attached policies.
paEffectivePermission :: Lens' PublicAccess (Maybe Text)
paEffectivePermission = lens _paEffectivePermission (\s a -> s {_paEffectivePermission = a})

instance FromJSON PublicAccess where
  parseJSON =
    withObject
      "PublicAccess"
      ( \x ->
          PublicAccess'
            <$> (x .:? "permissionConfiguration")
            <*> (x .:? "effectivePermission")
      )

instance Hashable PublicAccess

instance NFData PublicAccess
