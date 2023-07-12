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
-- Module      : Amazonka.EC2.Types.AdditionalDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AdditionalDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AnalysisComponent
import qualified Amazonka.Prelude as Prelude

-- | Describes an additional detail for a path analysis.
--
-- /See:/ 'newAdditionalDetail' smart constructor.
data AdditionalDetail = AdditionalDetail'
  { -- | The information type.
    additionalDetailType :: Prelude.Maybe Prelude.Text,
    -- | The path component.
    component :: Prelude.Maybe AnalysisComponent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdditionalDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDetailType', 'additionalDetail_additionalDetailType' - The information type.
--
-- 'component', 'additionalDetail_component' - The path component.
newAdditionalDetail ::
  AdditionalDetail
newAdditionalDetail =
  AdditionalDetail'
    { additionalDetailType =
        Prelude.Nothing,
      component = Prelude.Nothing
    }

-- | The information type.
additionalDetail_additionalDetailType :: Lens.Lens' AdditionalDetail (Prelude.Maybe Prelude.Text)
additionalDetail_additionalDetailType = Lens.lens (\AdditionalDetail' {additionalDetailType} -> additionalDetailType) (\s@AdditionalDetail' {} a -> s {additionalDetailType = a} :: AdditionalDetail)

-- | The path component.
additionalDetail_component :: Lens.Lens' AdditionalDetail (Prelude.Maybe AnalysisComponent)
additionalDetail_component = Lens.lens (\AdditionalDetail' {component} -> component) (\s@AdditionalDetail' {} a -> s {component = a} :: AdditionalDetail)

instance Data.FromXML AdditionalDetail where
  parseXML x =
    AdditionalDetail'
      Prelude.<$> (x Data..@? "additionalDetailType")
      Prelude.<*> (x Data..@? "component")

instance Prelude.Hashable AdditionalDetail where
  hashWithSalt _salt AdditionalDetail' {..} =
    _salt
      `Prelude.hashWithSalt` additionalDetailType
      `Prelude.hashWithSalt` component

instance Prelude.NFData AdditionalDetail where
  rnf AdditionalDetail' {..} =
    Prelude.rnf additionalDetailType
      `Prelude.seq` Prelude.rnf component
