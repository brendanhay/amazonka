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
-- Module      : Amazonka.AppStream.Types.EntitlementAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.EntitlementAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An attribute associated with an entitlement. Application entitlements
-- work by matching a supported SAML 2.0 attribute name to a value when a
-- user identity federates to an Amazon AppStream 2.0 SAML application.
--
-- /See:/ 'newEntitlementAttribute' smart constructor.
data EntitlementAttribute = EntitlementAttribute'
  { -- | A supported AWS IAM SAML @PrincipalTag@ attribute that is matched to the
    -- associated value when a user identity federates into an Amazon AppStream
    -- 2.0 SAML application.
    --
    -- The following are valid values:
    --
    -- -   roles
    --
    -- -   department
    --
    -- -   organization
    --
    -- -   groups
    --
    -- -   title
    --
    -- -   costCenter
    --
    -- -   userType
    name :: Prelude.Text,
    -- | A value that is matched to a supported SAML attribute name when a user
    -- identity federates into an Amazon AppStream 2.0 SAML application.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntitlementAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'entitlementAttribute_name' - A supported AWS IAM SAML @PrincipalTag@ attribute that is matched to the
-- associated value when a user identity federates into an Amazon AppStream
-- 2.0 SAML application.
--
-- The following are valid values:
--
-- -   roles
--
-- -   department
--
-- -   organization
--
-- -   groups
--
-- -   title
--
-- -   costCenter
--
-- -   userType
--
-- 'value', 'entitlementAttribute_value' - A value that is matched to a supported SAML attribute name when a user
-- identity federates into an Amazon AppStream 2.0 SAML application.
newEntitlementAttribute ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  EntitlementAttribute
newEntitlementAttribute pName_ pValue_ =
  EntitlementAttribute'
    { name = pName_,
      value = pValue_
    }

-- | A supported AWS IAM SAML @PrincipalTag@ attribute that is matched to the
-- associated value when a user identity federates into an Amazon AppStream
-- 2.0 SAML application.
--
-- The following are valid values:
--
-- -   roles
--
-- -   department
--
-- -   organization
--
-- -   groups
--
-- -   title
--
-- -   costCenter
--
-- -   userType
entitlementAttribute_name :: Lens.Lens' EntitlementAttribute Prelude.Text
entitlementAttribute_name = Lens.lens (\EntitlementAttribute' {name} -> name) (\s@EntitlementAttribute' {} a -> s {name = a} :: EntitlementAttribute)

-- | A value that is matched to a supported SAML attribute name when a user
-- identity federates into an Amazon AppStream 2.0 SAML application.
entitlementAttribute_value :: Lens.Lens' EntitlementAttribute Prelude.Text
entitlementAttribute_value = Lens.lens (\EntitlementAttribute' {value} -> value) (\s@EntitlementAttribute' {} a -> s {value = a} :: EntitlementAttribute)

instance Core.FromJSON EntitlementAttribute where
  parseJSON =
    Core.withObject
      "EntitlementAttribute"
      ( \x ->
          EntitlementAttribute'
            Prelude.<$> (x Core..: "Name") Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable EntitlementAttribute where
  hashWithSalt _salt EntitlementAttribute' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData EntitlementAttribute where
  rnf EntitlementAttribute' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Core.ToJSON EntitlementAttribute where
  toJSON EntitlementAttribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Value" Core..= value)
          ]
      )
