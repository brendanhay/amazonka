{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams where

import Network.AWS.IoT.Types.PolicyTemplateName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Parameters to define a mitigation action that adds a blank policy to
-- restrict permissions.
--
-- /See:/ 'newReplaceDefaultPolicyVersionParams' smart constructor.
data ReplaceDefaultPolicyVersionParams = ReplaceDefaultPolicyVersionParams'
  { -- | The name of the template to be applied. The only supported value is
    -- @BLANK_POLICY@.
    templateName :: PolicyTemplateName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    ReplaceDefaultPolicyVersionParams
  where
  parseJSON =
    Prelude.withObject
      "ReplaceDefaultPolicyVersionParams"
      ( \x ->
          ReplaceDefaultPolicyVersionParams'
            Prelude.<$> (x Prelude..: "templateName")
      )

instance
  Prelude.Hashable
    ReplaceDefaultPolicyVersionParams

instance
  Prelude.NFData
    ReplaceDefaultPolicyVersionParams

instance
  Prelude.ToJSON
    ReplaceDefaultPolicyVersionParams
  where
  toJSON ReplaceDefaultPolicyVersionParams' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("templateName" Prelude..= templateName)
          ]
      )
