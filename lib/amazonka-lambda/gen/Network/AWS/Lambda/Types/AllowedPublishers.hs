{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AllowedPublishers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AllowedPublishers where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | List of signing profiles that can sign a code package.
--
--
--
-- /See:/ 'allowedPublishers' smart constructor.
newtype AllowedPublishers = AllowedPublishers'
  { _apSigningProfileVersionARNs ::
      List1 Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllowedPublishers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apSigningProfileVersionARNs' - The Amazon Resource Name (ARN) for each of the signing profiles. A signing profile defines a trusted user who can sign a code package.
allowedPublishers ::
  -- | 'apSigningProfileVersionARNs'
  NonEmpty Text ->
  AllowedPublishers
allowedPublishers pSigningProfileVersionARNs_ =
  AllowedPublishers'
    { _apSigningProfileVersionARNs =
        _List1 # pSigningProfileVersionARNs_
    }

-- | The Amazon Resource Name (ARN) for each of the signing profiles. A signing profile defines a trusted user who can sign a code package.
apSigningProfileVersionARNs :: Lens' AllowedPublishers (NonEmpty Text)
apSigningProfileVersionARNs = lens _apSigningProfileVersionARNs (\s a -> s {_apSigningProfileVersionARNs = a}) . _List1

instance FromJSON AllowedPublishers where
  parseJSON =
    withObject
      "AllowedPublishers"
      (\x -> AllowedPublishers' <$> (x .: "SigningProfileVersionArns"))

instance Hashable AllowedPublishers

instance NFData AllowedPublishers

instance ToJSON AllowedPublishers where
  toJSON AllowedPublishers' {..} =
    object
      ( catMaybes
          [ Just
              ("SigningProfileVersionArns" .= _apSigningProfileVersionARNs)
          ]
      )
