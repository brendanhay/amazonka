{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.OpenIdConnectProviderListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.OpenIdConnectProviderListEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect provider.
--
--
--
-- /See:/ 'openIdConnectProviderListEntry' smart constructor.
newtype OpenIdConnectProviderListEntry = OpenIdConnectProviderListEntry'
  { _oicpleARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpenIdConnectProviderListEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oicpleARN' - Undocumented member.
openIdConnectProviderListEntry ::
  OpenIdConnectProviderListEntry
openIdConnectProviderListEntry =
  OpenIdConnectProviderListEntry' {_oicpleARN = Nothing}

-- | Undocumented member.
oicpleARN :: Lens' OpenIdConnectProviderListEntry (Maybe Text)
oicpleARN = lens _oicpleARN (\s a -> s {_oicpleARN = a})

instance FromXML OpenIdConnectProviderListEntry where
  parseXML x = OpenIdConnectProviderListEntry' <$> (x .@? "Arn")

instance Hashable OpenIdConnectProviderListEntry

instance NFData OpenIdConnectProviderListEntry
