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
-- Module      : Amazonka.SSOAdmin.Types.AccessControlAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.AccessControlAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSOAdmin.Types.AccessControlAttributeValue

-- | These are IAM Identity Center identity store attributes that you can
-- configure for use in attributes-based access control (ABAC). You can
-- create permissions policies that determine who can access your AWS
-- resources based upon the configured attribute values. When you enable
-- ABAC and specify @AccessControlAttributes@, IAM Identity Center passes
-- the attribute values of the authenticated user into IAM for use in
-- policy evaluation.
--
-- /See:/ 'newAccessControlAttribute' smart constructor.
data AccessControlAttribute = AccessControlAttribute'
  { -- | The name of the attribute associated with your identities in your
    -- identity source. This is used to map a specified attribute in your
    -- identity source with an attribute in IAM Identity Center.
    key :: Prelude.Text,
    -- | The value used for mapping a specified attribute to an identity source.
    value :: AccessControlAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessControlAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'accessControlAttribute_key' - The name of the attribute associated with your identities in your
-- identity source. This is used to map a specified attribute in your
-- identity source with an attribute in IAM Identity Center.
--
-- 'value', 'accessControlAttribute_value' - The value used for mapping a specified attribute to an identity source.
newAccessControlAttribute ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  AccessControlAttributeValue ->
  AccessControlAttribute
newAccessControlAttribute pKey_ pValue_ =
  AccessControlAttribute'
    { key = pKey_,
      value = pValue_
    }

-- | The name of the attribute associated with your identities in your
-- identity source. This is used to map a specified attribute in your
-- identity source with an attribute in IAM Identity Center.
accessControlAttribute_key :: Lens.Lens' AccessControlAttribute Prelude.Text
accessControlAttribute_key = Lens.lens (\AccessControlAttribute' {key} -> key) (\s@AccessControlAttribute' {} a -> s {key = a} :: AccessControlAttribute)

-- | The value used for mapping a specified attribute to an identity source.
accessControlAttribute_value :: Lens.Lens' AccessControlAttribute AccessControlAttributeValue
accessControlAttribute_value = Lens.lens (\AccessControlAttribute' {value} -> value) (\s@AccessControlAttribute' {} a -> s {value = a} :: AccessControlAttribute)

instance Data.FromJSON AccessControlAttribute where
  parseJSON =
    Data.withObject
      "AccessControlAttribute"
      ( \x ->
          AccessControlAttribute'
            Prelude.<$> (x Data..: "Key") Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable AccessControlAttribute where
  hashWithSalt _salt AccessControlAttribute' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData AccessControlAttribute where
  rnf AccessControlAttribute' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON AccessControlAttribute where
  toJSON AccessControlAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Value" Data..= value)
          ]
      )
