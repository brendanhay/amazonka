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
-- Module      : Network.AWS.SESv2.Types.SuppressionListDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.SuppressionListDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.SuppressionListImportAction

-- | An object that contains details about the action of suppression list.
--
-- /See:/ 'newSuppressionListDestination' smart constructor.
data SuppressionListDestination = SuppressionListDestination'
  { -- | The type of action that you want to perform on the address. Acceptable
    -- values:
    --
    -- -   PUT: add the addresses to the suppression list. If the record
    --     already exists, it will override it with the new value.
    --
    -- -   DELETE: remove the addresses from the suppression list.
    suppressionListImportAction :: SuppressionListImportAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuppressionListDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suppressionListImportAction', 'suppressionListDestination_suppressionListImportAction' - The type of action that you want to perform on the address. Acceptable
-- values:
--
-- -   PUT: add the addresses to the suppression list. If the record
--     already exists, it will override it with the new value.
--
-- -   DELETE: remove the addresses from the suppression list.
newSuppressionListDestination ::
  -- | 'suppressionListImportAction'
  SuppressionListImportAction ->
  SuppressionListDestination
newSuppressionListDestination
  pSuppressionListImportAction_ =
    SuppressionListDestination'
      { suppressionListImportAction =
          pSuppressionListImportAction_
      }

-- | The type of action that you want to perform on the address. Acceptable
-- values:
--
-- -   PUT: add the addresses to the suppression list. If the record
--     already exists, it will override it with the new value.
--
-- -   DELETE: remove the addresses from the suppression list.
suppressionListDestination_suppressionListImportAction :: Lens.Lens' SuppressionListDestination SuppressionListImportAction
suppressionListDestination_suppressionListImportAction = Lens.lens (\SuppressionListDestination' {suppressionListImportAction} -> suppressionListImportAction) (\s@SuppressionListDestination' {} a -> s {suppressionListImportAction = a} :: SuppressionListDestination)

instance Core.FromJSON SuppressionListDestination where
  parseJSON =
    Core.withObject
      "SuppressionListDestination"
      ( \x ->
          SuppressionListDestination'
            Prelude.<$> (x Core..: "SuppressionListImportAction")
      )

instance Prelude.Hashable SuppressionListDestination

instance Prelude.NFData SuppressionListDestination

instance Core.ToJSON SuppressionListDestination where
  toJSON SuppressionListDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SuppressionListImportAction"
                  Core..= suppressionListImportAction
              )
          ]
      )
