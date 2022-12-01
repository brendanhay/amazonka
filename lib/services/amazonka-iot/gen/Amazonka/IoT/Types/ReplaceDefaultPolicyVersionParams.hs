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
-- Module      : Amazonka.IoT.Types.ReplaceDefaultPolicyVersionParams
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ReplaceDefaultPolicyVersionParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.PolicyTemplateName
import qualified Amazonka.Prelude as Prelude

-- | Parameters to define a mitigation action that adds a blank policy to
-- restrict permissions.
--
-- /See:/ 'newReplaceDefaultPolicyVersionParams' smart constructor.
data ReplaceDefaultPolicyVersionParams = ReplaceDefaultPolicyVersionParams'
  { -- | The name of the template to be applied. The only supported value is
    -- @BLANK_POLICY@.
    templateName :: PolicyTemplateName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceDefaultPolicyVersionParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'replaceDefaultPolicyVersionParams_templateName' - The name of the template to be applied. The only supported value is
-- @BLANK_POLICY@.
newReplaceDefaultPolicyVersionParams ::
  -- | 'templateName'
  PolicyTemplateName ->
  ReplaceDefaultPolicyVersionParams
newReplaceDefaultPolicyVersionParams pTemplateName_ =
  ReplaceDefaultPolicyVersionParams'
    { templateName =
        pTemplateName_
    }

-- | The name of the template to be applied. The only supported value is
-- @BLANK_POLICY@.
replaceDefaultPolicyVersionParams_templateName :: Lens.Lens' ReplaceDefaultPolicyVersionParams PolicyTemplateName
replaceDefaultPolicyVersionParams_templateName = Lens.lens (\ReplaceDefaultPolicyVersionParams' {templateName} -> templateName) (\s@ReplaceDefaultPolicyVersionParams' {} a -> s {templateName = a} :: ReplaceDefaultPolicyVersionParams)

instance
  Core.FromJSON
    ReplaceDefaultPolicyVersionParams
  where
  parseJSON =
    Core.withObject
      "ReplaceDefaultPolicyVersionParams"
      ( \x ->
          ReplaceDefaultPolicyVersionParams'
            Prelude.<$> (x Core..: "templateName")
      )

instance
  Prelude.Hashable
    ReplaceDefaultPolicyVersionParams
  where
  hashWithSalt
    _salt
    ReplaceDefaultPolicyVersionParams' {..} =
      _salt `Prelude.hashWithSalt` templateName

instance
  Prelude.NFData
    ReplaceDefaultPolicyVersionParams
  where
  rnf ReplaceDefaultPolicyVersionParams' {..} =
    Prelude.rnf templateName

instance
  Core.ToJSON
    ReplaceDefaultPolicyVersionParams
  where
  toJSON ReplaceDefaultPolicyVersionParams' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("templateName" Core..= templateName)]
      )
