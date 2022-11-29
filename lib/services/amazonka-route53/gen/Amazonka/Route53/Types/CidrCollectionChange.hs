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
-- Module      : Amazonka.Route53.Types.CidrCollectionChange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.CidrCollectionChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.CidrCollectionChangeAction

-- | A complex type that contains information about the CIDR collection
-- change.
--
-- /See:/ 'newCidrCollectionChange' smart constructor.
data CidrCollectionChange = CidrCollectionChange'
  { -- | Name of the location that is associated with the CIDR collection.
    locationName :: Prelude.Text,
    -- | CIDR collection change action.
    action :: CidrCollectionChangeAction,
    -- | List of CIDR blocks.
    cidrList :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CidrCollectionChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationName', 'cidrCollectionChange_locationName' - Name of the location that is associated with the CIDR collection.
--
-- 'action', 'cidrCollectionChange_action' - CIDR collection change action.
--
-- 'cidrList', 'cidrCollectionChange_cidrList' - List of CIDR blocks.
newCidrCollectionChange ::
  -- | 'locationName'
  Prelude.Text ->
  -- | 'action'
  CidrCollectionChangeAction ->
  -- | 'cidrList'
  Prelude.NonEmpty Prelude.Text ->
  CidrCollectionChange
newCidrCollectionChange
  pLocationName_
  pAction_
  pCidrList_ =
    CidrCollectionChange'
      { locationName =
          pLocationName_,
        action = pAction_,
        cidrList = Lens.coerced Lens.# pCidrList_
      }

-- | Name of the location that is associated with the CIDR collection.
cidrCollectionChange_locationName :: Lens.Lens' CidrCollectionChange Prelude.Text
cidrCollectionChange_locationName = Lens.lens (\CidrCollectionChange' {locationName} -> locationName) (\s@CidrCollectionChange' {} a -> s {locationName = a} :: CidrCollectionChange)

-- | CIDR collection change action.
cidrCollectionChange_action :: Lens.Lens' CidrCollectionChange CidrCollectionChangeAction
cidrCollectionChange_action = Lens.lens (\CidrCollectionChange' {action} -> action) (\s@CidrCollectionChange' {} a -> s {action = a} :: CidrCollectionChange)

-- | List of CIDR blocks.
cidrCollectionChange_cidrList :: Lens.Lens' CidrCollectionChange (Prelude.NonEmpty Prelude.Text)
cidrCollectionChange_cidrList = Lens.lens (\CidrCollectionChange' {cidrList} -> cidrList) (\s@CidrCollectionChange' {} a -> s {cidrList = a} :: CidrCollectionChange) Prelude.. Lens.coerced

instance Prelude.Hashable CidrCollectionChange where
  hashWithSalt _salt CidrCollectionChange' {..} =
    _salt `Prelude.hashWithSalt` locationName
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` cidrList

instance Prelude.NFData CidrCollectionChange where
  rnf CidrCollectionChange' {..} =
    Prelude.rnf locationName
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf cidrList

instance Core.ToXML CidrCollectionChange where
  toXML CidrCollectionChange' {..} =
    Prelude.mconcat
      [ "LocationName" Core.@= locationName,
        "Action" Core.@= action,
        "CidrList" Core.@= Core.toXMLList "Cidr" cidrList
      ]
