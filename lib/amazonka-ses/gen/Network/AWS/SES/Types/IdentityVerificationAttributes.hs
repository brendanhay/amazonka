{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityVerificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityVerificationAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.VerificationStatus

-- | Represents the verification attributes of a single identity.
--
--
--
-- /See:/ 'identityVerificationAttributes' smart constructor.
data IdentityVerificationAttributes = IdentityVerificationAttributes'
  { _ivaVerificationToken ::
      !(Maybe Text),
    _ivaVerificationStatus ::
      !VerificationStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IdentityVerificationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivaVerificationToken' - The verification token for a domain identity. Null for email address identities.
--
-- * 'ivaVerificationStatus' - The verification status of the identity: "Pending", "Success", "Failed", or "TemporaryFailure".
identityVerificationAttributes ::
  -- | 'ivaVerificationStatus'
  VerificationStatus ->
  IdentityVerificationAttributes
identityVerificationAttributes pVerificationStatus_ =
  IdentityVerificationAttributes'
    { _ivaVerificationToken = Nothing,
      _ivaVerificationStatus = pVerificationStatus_
    }

-- | The verification token for a domain identity. Null for email address identities.
ivaVerificationToken :: Lens' IdentityVerificationAttributes (Maybe Text)
ivaVerificationToken = lens _ivaVerificationToken (\s a -> s {_ivaVerificationToken = a})

-- | The verification status of the identity: "Pending", "Success", "Failed", or "TemporaryFailure".
ivaVerificationStatus :: Lens' IdentityVerificationAttributes VerificationStatus
ivaVerificationStatus = lens _ivaVerificationStatus (\s a -> s {_ivaVerificationStatus = a})

instance FromXML IdentityVerificationAttributes where
  parseXML x =
    IdentityVerificationAttributes'
      <$> (x .@? "VerificationToken") <*> (x .@ "VerificationStatus")

instance Hashable IdentityVerificationAttributes

instance NFData IdentityVerificationAttributes
