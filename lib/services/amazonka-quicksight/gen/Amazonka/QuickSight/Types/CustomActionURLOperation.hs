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
-- Module      : Amazonka.QuickSight.Types.CustomActionURLOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomActionURLOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.URLTargetConfiguration

-- | The URL operation that opens a link to another webpage.
--
-- /See:/ 'newCustomActionURLOperation' smart constructor.
data CustomActionURLOperation = CustomActionURLOperation'
  { -- | THe URL link of the @CustomActionURLOperation@.
    uRLTemplate :: Prelude.Text,
    -- | The target of the @CustomActionURLOperation@.
    --
    -- Valid values are defined as follows:
    --
    -- -   @NEW_TAB@: Opens the target URL in a new browser tab.
    --
    -- -   @NEW_WINDOW@: Opens the target URL in a new browser window.
    --
    -- -   @SAME_TAB@: Opens the target URL in the same browser tab.
    uRLTarget :: URLTargetConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomActionURLOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uRLTemplate', 'customActionURLOperation_uRLTemplate' - THe URL link of the @CustomActionURLOperation@.
--
-- 'uRLTarget', 'customActionURLOperation_uRLTarget' - The target of the @CustomActionURLOperation@.
--
-- Valid values are defined as follows:
--
-- -   @NEW_TAB@: Opens the target URL in a new browser tab.
--
-- -   @NEW_WINDOW@: Opens the target URL in a new browser window.
--
-- -   @SAME_TAB@: Opens the target URL in the same browser tab.
newCustomActionURLOperation ::
  -- | 'uRLTemplate'
  Prelude.Text ->
  -- | 'uRLTarget'
  URLTargetConfiguration ->
  CustomActionURLOperation
newCustomActionURLOperation pURLTemplate_ pURLTarget_ =
  CustomActionURLOperation'
    { uRLTemplate =
        pURLTemplate_,
      uRLTarget = pURLTarget_
    }

-- | THe URL link of the @CustomActionURLOperation@.
customActionURLOperation_uRLTemplate :: Lens.Lens' CustomActionURLOperation Prelude.Text
customActionURLOperation_uRLTemplate = Lens.lens (\CustomActionURLOperation' {uRLTemplate} -> uRLTemplate) (\s@CustomActionURLOperation' {} a -> s {uRLTemplate = a} :: CustomActionURLOperation)

-- | The target of the @CustomActionURLOperation@.
--
-- Valid values are defined as follows:
--
-- -   @NEW_TAB@: Opens the target URL in a new browser tab.
--
-- -   @NEW_WINDOW@: Opens the target URL in a new browser window.
--
-- -   @SAME_TAB@: Opens the target URL in the same browser tab.
customActionURLOperation_uRLTarget :: Lens.Lens' CustomActionURLOperation URLTargetConfiguration
customActionURLOperation_uRLTarget = Lens.lens (\CustomActionURLOperation' {uRLTarget} -> uRLTarget) (\s@CustomActionURLOperation' {} a -> s {uRLTarget = a} :: CustomActionURLOperation)

instance Data.FromJSON CustomActionURLOperation where
  parseJSON =
    Data.withObject
      "CustomActionURLOperation"
      ( \x ->
          CustomActionURLOperation'
            Prelude.<$> (x Data..: "URLTemplate")
            Prelude.<*> (x Data..: "URLTarget")
      )

instance Prelude.Hashable CustomActionURLOperation where
  hashWithSalt _salt CustomActionURLOperation' {..} =
    _salt
      `Prelude.hashWithSalt` uRLTemplate
      `Prelude.hashWithSalt` uRLTarget

instance Prelude.NFData CustomActionURLOperation where
  rnf CustomActionURLOperation' {..} =
    Prelude.rnf uRLTemplate
      `Prelude.seq` Prelude.rnf uRLTarget

instance Data.ToJSON CustomActionURLOperation where
  toJSON CustomActionURLOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("URLTemplate" Data..= uRLTemplate),
            Prelude.Just ("URLTarget" Data..= uRLTarget)
          ]
      )
