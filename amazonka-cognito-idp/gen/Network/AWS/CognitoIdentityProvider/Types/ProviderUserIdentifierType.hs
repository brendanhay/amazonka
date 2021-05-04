{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A container for information about an identity provider for a user pool.
--
-- /See:/ 'newProviderUserIdentifierType' smart constructor.
data ProviderUserIdentifierType = ProviderUserIdentifierType'
  { -- | The name of the provider, for example, Facebook, Google, or Login with
    -- Amazon.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The name of the provider attribute to link to, for example, @NameID@.
    providerAttributeName :: Prelude.Maybe Prelude.Text,
    -- | The value of the provider attribute to link to, for example,
    -- @xxxxx_account@.
    providerAttributeValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProviderUserIdentifierType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'providerName', 'providerUserIdentifierType_providerName' - The name of the provider, for example, Facebook, Google, or Login with
-- Amazon.
--
-- 'providerAttributeName', 'providerUserIdentifierType_providerAttributeName' - The name of the provider attribute to link to, for example, @NameID@.
--
-- 'providerAttributeValue', 'providerUserIdentifierType_providerAttributeValue' - The value of the provider attribute to link to, for example,
-- @xxxxx_account@.
newProviderUserIdentifierType ::
  ProviderUserIdentifierType
newProviderUserIdentifierType =
  ProviderUserIdentifierType'
    { providerName =
        Prelude.Nothing,
      providerAttributeName = Prelude.Nothing,
      providerAttributeValue = Prelude.Nothing
    }

-- | The name of the provider, for example, Facebook, Google, or Login with
-- Amazon.
providerUserIdentifierType_providerName :: Lens.Lens' ProviderUserIdentifierType (Prelude.Maybe Prelude.Text)
providerUserIdentifierType_providerName = Lens.lens (\ProviderUserIdentifierType' {providerName} -> providerName) (\s@ProviderUserIdentifierType' {} a -> s {providerName = a} :: ProviderUserIdentifierType)

-- | The name of the provider attribute to link to, for example, @NameID@.
providerUserIdentifierType_providerAttributeName :: Lens.Lens' ProviderUserIdentifierType (Prelude.Maybe Prelude.Text)
providerUserIdentifierType_providerAttributeName = Lens.lens (\ProviderUserIdentifierType' {providerAttributeName} -> providerAttributeName) (\s@ProviderUserIdentifierType' {} a -> s {providerAttributeName = a} :: ProviderUserIdentifierType)

-- | The value of the provider attribute to link to, for example,
-- @xxxxx_account@.
providerUserIdentifierType_providerAttributeValue :: Lens.Lens' ProviderUserIdentifierType (Prelude.Maybe Prelude.Text)
providerUserIdentifierType_providerAttributeValue = Lens.lens (\ProviderUserIdentifierType' {providerAttributeValue} -> providerAttributeValue) (\s@ProviderUserIdentifierType' {} a -> s {providerAttributeValue = a} :: ProviderUserIdentifierType)

instance Prelude.Hashable ProviderUserIdentifierType

instance Prelude.NFData ProviderUserIdentifierType

instance Prelude.ToJSON ProviderUserIdentifierType where
  toJSON ProviderUserIdentifierType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ProviderName" Prelude..=)
              Prelude.<$> providerName,
            ("ProviderAttributeName" Prelude..=)
              Prelude.<$> providerAttributeName,
            ("ProviderAttributeValue" Prelude..=)
              Prelude.<$> providerAttributeValue
          ]
      )
