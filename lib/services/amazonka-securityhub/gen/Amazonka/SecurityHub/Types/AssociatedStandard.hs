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
-- Module      : Amazonka.SecurityHub.Types.AssociatedStandard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AssociatedStandard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an enabled security standard in which a security
-- control is enabled.
--
-- /See:/ 'newAssociatedStandard' smart constructor.
data AssociatedStandard = AssociatedStandard'
  { -- | The unique identifier of a standard in which a control is enabled. This
    -- field consists of the resource portion of the Amazon Resource Name (ARN)
    -- returned for a standard in the
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_DescribeStandards.html DescribeStandards>
    -- API response.
    standardsId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedStandard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardsId', 'associatedStandard_standardsId' - The unique identifier of a standard in which a control is enabled. This
-- field consists of the resource portion of the Amazon Resource Name (ARN)
-- returned for a standard in the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_DescribeStandards.html DescribeStandards>
-- API response.
newAssociatedStandard ::
  AssociatedStandard
newAssociatedStandard =
  AssociatedStandard' {standardsId = Prelude.Nothing}

-- | The unique identifier of a standard in which a control is enabled. This
-- field consists of the resource portion of the Amazon Resource Name (ARN)
-- returned for a standard in the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_DescribeStandards.html DescribeStandards>
-- API response.
associatedStandard_standardsId :: Lens.Lens' AssociatedStandard (Prelude.Maybe Prelude.Text)
associatedStandard_standardsId = Lens.lens (\AssociatedStandard' {standardsId} -> standardsId) (\s@AssociatedStandard' {} a -> s {standardsId = a} :: AssociatedStandard)

instance Data.FromJSON AssociatedStandard where
  parseJSON =
    Data.withObject
      "AssociatedStandard"
      ( \x ->
          AssociatedStandard'
            Prelude.<$> (x Data..:? "StandardsId")
      )

instance Prelude.Hashable AssociatedStandard where
  hashWithSalt _salt AssociatedStandard' {..} =
    _salt `Prelude.hashWithSalt` standardsId

instance Prelude.NFData AssociatedStandard where
  rnf AssociatedStandard' {..} = Prelude.rnf standardsId

instance Data.ToJSON AssociatedStandard where
  toJSON AssociatedStandard' {..} =
    Data.object
      ( Prelude.catMaybes
          [("StandardsId" Data..=) Prelude.<$> standardsId]
      )
