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
-- Module      : Amazonka.EC2.Types.TransitGatewayPolicyRuleMetaData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayPolicyRuleMetaData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the meta data tags associated with a transit gateway policy
-- rule.
--
-- /See:/ 'newTransitGatewayPolicyRuleMetaData' smart constructor.
data TransitGatewayPolicyRuleMetaData = TransitGatewayPolicyRuleMetaData'
  { -- | The value of the key for the transit gateway policy rule meta data tag.
    metaDataValue :: Prelude.Maybe Prelude.Text,
    -- | The key name for the transit gateway policy rule meta data tag.
    metaDataKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayPolicyRuleMetaData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metaDataValue', 'transitGatewayPolicyRuleMetaData_metaDataValue' - The value of the key for the transit gateway policy rule meta data tag.
--
-- 'metaDataKey', 'transitGatewayPolicyRuleMetaData_metaDataKey' - The key name for the transit gateway policy rule meta data tag.
newTransitGatewayPolicyRuleMetaData ::
  TransitGatewayPolicyRuleMetaData
newTransitGatewayPolicyRuleMetaData =
  TransitGatewayPolicyRuleMetaData'
    { metaDataValue =
        Prelude.Nothing,
      metaDataKey = Prelude.Nothing
    }

-- | The value of the key for the transit gateway policy rule meta data tag.
transitGatewayPolicyRuleMetaData_metaDataValue :: Lens.Lens' TransitGatewayPolicyRuleMetaData (Prelude.Maybe Prelude.Text)
transitGatewayPolicyRuleMetaData_metaDataValue = Lens.lens (\TransitGatewayPolicyRuleMetaData' {metaDataValue} -> metaDataValue) (\s@TransitGatewayPolicyRuleMetaData' {} a -> s {metaDataValue = a} :: TransitGatewayPolicyRuleMetaData)

-- | The key name for the transit gateway policy rule meta data tag.
transitGatewayPolicyRuleMetaData_metaDataKey :: Lens.Lens' TransitGatewayPolicyRuleMetaData (Prelude.Maybe Prelude.Text)
transitGatewayPolicyRuleMetaData_metaDataKey = Lens.lens (\TransitGatewayPolicyRuleMetaData' {metaDataKey} -> metaDataKey) (\s@TransitGatewayPolicyRuleMetaData' {} a -> s {metaDataKey = a} :: TransitGatewayPolicyRuleMetaData)

instance
  Data.FromXML
    TransitGatewayPolicyRuleMetaData
  where
  parseXML x =
    TransitGatewayPolicyRuleMetaData'
      Prelude.<$> (x Data..@? "metaDataValue")
      Prelude.<*> (x Data..@? "metaDataKey")

instance
  Prelude.Hashable
    TransitGatewayPolicyRuleMetaData
  where
  hashWithSalt
    _salt
    TransitGatewayPolicyRuleMetaData' {..} =
      _salt `Prelude.hashWithSalt` metaDataValue
        `Prelude.hashWithSalt` metaDataKey

instance
  Prelude.NFData
    TransitGatewayPolicyRuleMetaData
  where
  rnf TransitGatewayPolicyRuleMetaData' {..} =
    Prelude.rnf metaDataValue
      `Prelude.seq` Prelude.rnf metaDataKey
