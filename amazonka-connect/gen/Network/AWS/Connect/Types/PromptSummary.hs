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
-- Module      : Network.AWS.Connect.Types.PromptSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PromptSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the prompt.
--
-- /See:/ 'newPromptSummary' smart constructor.
data PromptSummary = PromptSummary'
  { -- | The Amazon Resource Name (ARN) of the prompt.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the prompt.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the prompt.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PromptSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'promptSummary_arn' - The Amazon Resource Name (ARN) of the prompt.
--
-- 'id', 'promptSummary_id' - The identifier of the prompt.
--
-- 'name', 'promptSummary_name' - The name of the prompt.
newPromptSummary ::
  PromptSummary
newPromptSummary =
  PromptSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the prompt.
promptSummary_arn :: Lens.Lens' PromptSummary (Prelude.Maybe Prelude.Text)
promptSummary_arn = Lens.lens (\PromptSummary' {arn} -> arn) (\s@PromptSummary' {} a -> s {arn = a} :: PromptSummary)

-- | The identifier of the prompt.
promptSummary_id :: Lens.Lens' PromptSummary (Prelude.Maybe Prelude.Text)
promptSummary_id = Lens.lens (\PromptSummary' {id} -> id) (\s@PromptSummary' {} a -> s {id = a} :: PromptSummary)

-- | The name of the prompt.
promptSummary_name :: Lens.Lens' PromptSummary (Prelude.Maybe Prelude.Text)
promptSummary_name = Lens.lens (\PromptSummary' {name} -> name) (\s@PromptSummary' {} a -> s {name = a} :: PromptSummary)

instance Prelude.FromJSON PromptSummary where
  parseJSON =
    Prelude.withObject
      "PromptSummary"
      ( \x ->
          PromptSummary'
            Prelude.<$> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable PromptSummary

instance Prelude.NFData PromptSummary
