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
-- Module      : Amazonka.FIS.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ActionParameter
import Amazonka.FIS.Types.ActionTarget
import qualified Amazonka.Prelude as Prelude

-- | Describes an action. For more information, see
-- <https://docs.aws.amazon.com/fis/latest/userguide/fis-actions-reference.html FIS actions>
-- in the /Fault Injection Simulator User Guide/.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | The description for the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the action.
    id :: Prelude.Maybe Prelude.Text,
    -- | The action parameters, if applicable.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text ActionParameter),
    -- | The tags for the action.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The supported targets for the action.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text ActionTarget)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'action_description' - The description for the action.
--
-- 'id', 'action_id' - The ID of the action.
--
-- 'parameters', 'action_parameters' - The action parameters, if applicable.
--
-- 'tags', 'action_tags' - The tags for the action.
--
-- 'targets', 'action_targets' - The supported targets for the action.
newAction ::
  Action
newAction =
  Action'
    { description = Prelude.Nothing,
      id = Prelude.Nothing,
      parameters = Prelude.Nothing,
      tags = Prelude.Nothing,
      targets = Prelude.Nothing
    }

-- | The description for the action.
action_description :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_description = Lens.lens (\Action' {description} -> description) (\s@Action' {} a -> s {description = a} :: Action)

-- | The ID of the action.
action_id :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_id = Lens.lens (\Action' {id} -> id) (\s@Action' {} a -> s {id = a} :: Action)

-- | The action parameters, if applicable.
action_parameters :: Lens.Lens' Action (Prelude.Maybe (Prelude.HashMap Prelude.Text ActionParameter))
action_parameters = Lens.lens (\Action' {parameters} -> parameters) (\s@Action' {} a -> s {parameters = a} :: Action) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the action.
action_tags :: Lens.Lens' Action (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
action_tags = Lens.lens (\Action' {tags} -> tags) (\s@Action' {} a -> s {tags = a} :: Action) Prelude.. Lens.mapping Lens.coerced

-- | The supported targets for the action.
action_targets :: Lens.Lens' Action (Prelude.Maybe (Prelude.HashMap Prelude.Text ActionTarget))
action_targets = Lens.lens (\Action' {targets} -> targets) (\s@Action' {} a -> s {targets = a} :: Action) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "targets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targets

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targets
