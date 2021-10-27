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
-- Module      : Network.AWS.LexV2Models.Types.IntentClosingSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.IntentClosingSetting where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.ResponseSpecification
import qualified Network.AWS.Prelude as Prelude

-- | Provides a statement the Amazon Lex conveys to the user when the intent
-- is successfully fulfilled.
--
-- /See:/ 'newIntentClosingSetting' smart constructor.
data IntentClosingSetting = IntentClosingSetting'
  { -- | Specifies whether an intent\'s closing response is used. When this field
    -- is false, the closing response isn\'t sent to the user. If the @active@
    -- field isn\'t specified, the default is true.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The response that Amazon Lex sends to the user when the intent is
    -- complete.
    closingResponse :: ResponseSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntentClosingSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'intentClosingSetting_active' - Specifies whether an intent\'s closing response is used. When this field
-- is false, the closing response isn\'t sent to the user. If the @active@
-- field isn\'t specified, the default is true.
--
-- 'closingResponse', 'intentClosingSetting_closingResponse' - The response that Amazon Lex sends to the user when the intent is
-- complete.
newIntentClosingSetting ::
  -- | 'closingResponse'
  ResponseSpecification ->
  IntentClosingSetting
newIntentClosingSetting pClosingResponse_ =
  IntentClosingSetting'
    { active = Prelude.Nothing,
      closingResponse = pClosingResponse_
    }

-- | Specifies whether an intent\'s closing response is used. When this field
-- is false, the closing response isn\'t sent to the user. If the @active@
-- field isn\'t specified, the default is true.
intentClosingSetting_active :: Lens.Lens' IntentClosingSetting (Prelude.Maybe Prelude.Bool)
intentClosingSetting_active = Lens.lens (\IntentClosingSetting' {active} -> active) (\s@IntentClosingSetting' {} a -> s {active = a} :: IntentClosingSetting)

-- | The response that Amazon Lex sends to the user when the intent is
-- complete.
intentClosingSetting_closingResponse :: Lens.Lens' IntentClosingSetting ResponseSpecification
intentClosingSetting_closingResponse = Lens.lens (\IntentClosingSetting' {closingResponse} -> closingResponse) (\s@IntentClosingSetting' {} a -> s {closingResponse = a} :: IntentClosingSetting)

instance Core.FromJSON IntentClosingSetting where
  parseJSON =
    Core.withObject
      "IntentClosingSetting"
      ( \x ->
          IntentClosingSetting'
            Prelude.<$> (x Core..:? "active")
            Prelude.<*> (x Core..: "closingResponse")
      )

instance Prelude.Hashable IntentClosingSetting

instance Prelude.NFData IntentClosingSetting

instance Core.ToJSON IntentClosingSetting where
  toJSON IntentClosingSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("active" Core..=) Prelude.<$> active,
            Prelude.Just
              ("closingResponse" Core..= closingResponse)
          ]
      )
