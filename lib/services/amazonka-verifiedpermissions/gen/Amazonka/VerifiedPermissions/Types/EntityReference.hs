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
-- Module      : Amazonka.VerifiedPermissions.Types.EntityReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.EntityReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.EntityIdentifier

-- | Contains information about a principal or resource that can be
-- referenced in a Cedar policy.
--
-- This data type is used as part of the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_PolicyFilter.html PolicyFilter>
-- structure that is used as a request parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListPolicies.html ListPolicies>
-- operation..
--
-- /See:/ 'newEntityReference' smart constructor.
data EntityReference = EntityReference'
  { -- | The identifier of the entity. It can consist of either an EntityType and
    -- EntityId, a principal, or a resource.
    identifier :: Prelude.Maybe EntityIdentifier,
    -- | Used to indicate that a principal or resource is not specified. This can
    -- be used to search for policies that are not associated with a specific
    -- principal or resource.
    unspecified :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'entityReference_identifier' - The identifier of the entity. It can consist of either an EntityType and
-- EntityId, a principal, or a resource.
--
-- 'unspecified', 'entityReference_unspecified' - Used to indicate that a principal or resource is not specified. This can
-- be used to search for policies that are not associated with a specific
-- principal or resource.
newEntityReference ::
  EntityReference
newEntityReference =
  EntityReference'
    { identifier = Prelude.Nothing,
      unspecified = Prelude.Nothing
    }

-- | The identifier of the entity. It can consist of either an EntityType and
-- EntityId, a principal, or a resource.
entityReference_identifier :: Lens.Lens' EntityReference (Prelude.Maybe EntityIdentifier)
entityReference_identifier = Lens.lens (\EntityReference' {identifier} -> identifier) (\s@EntityReference' {} a -> s {identifier = a} :: EntityReference)

-- | Used to indicate that a principal or resource is not specified. This can
-- be used to search for policies that are not associated with a specific
-- principal or resource.
entityReference_unspecified :: Lens.Lens' EntityReference (Prelude.Maybe Prelude.Bool)
entityReference_unspecified = Lens.lens (\EntityReference' {unspecified} -> unspecified) (\s@EntityReference' {} a -> s {unspecified = a} :: EntityReference)

instance Prelude.Hashable EntityReference where
  hashWithSalt _salt EntityReference' {..} =
    _salt
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` unspecified

instance Prelude.NFData EntityReference where
  rnf EntityReference' {..} =
    Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf unspecified

instance Data.ToJSON EntityReference where
  toJSON EntityReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("identifier" Data..=) Prelude.<$> identifier,
            ("unspecified" Data..=) Prelude.<$> unspecified
          ]
      )
