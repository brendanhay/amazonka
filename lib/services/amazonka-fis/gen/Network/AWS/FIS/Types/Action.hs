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
-- Module      : Network.AWS.FIS.Types.Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FIS.Types.Action where

import qualified Network.AWS.Core as Core
import Network.AWS.FIS.Types.ActionParameter
import Network.AWS.FIS.Types.ActionTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an action. For more information, see
-- <https://docs.aws.amazon.com/fis/latest/userguide/fis-actions-reference.html AWS FIS actions>
-- in the /AWS Fault Injection Simulator User Guide/.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | The action parameters, if applicable.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text ActionParameter),
    -- | The supported targets for the action.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text ActionTarget),
    -- | The ID of the action.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description for the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags for the action.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'parameters', 'action_parameters' - The action parameters, if applicable.
--
-- 'targets', 'action_targets' - The supported targets for the action.
--
-- 'id', 'action_id' - The ID of the action.
--
-- 'description', 'action_description' - The description for the action.
--
-- 'tags', 'action_tags' - The tags for the action.
newAction ::
  Action
newAction =
  Action'
    { parameters = Prelude.Nothing,
      targets = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The action parameters, if applicable.
action_parameters :: Lens.Lens' Action (Prelude.Maybe (Prelude.HashMap Prelude.Text ActionParameter))
action_parameters = Lens.lens (\Action' {parameters} -> parameters) (\s@Action' {} a -> s {parameters = a} :: Action) Prelude.. Lens.mapping Lens.coerced

-- | The supported targets for the action.
action_targets :: Lens.Lens' Action (Prelude.Maybe (Prelude.HashMap Prelude.Text ActionTarget))
action_targets = Lens.lens (\Action' {targets} -> targets) (\s@Action' {} a -> s {targets = a} :: Action) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the action.
action_id :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_id = Lens.lens (\Action' {id} -> id) (\s@Action' {} a -> s {id = a} :: Action)

-- | The description for the action.
action_description :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_description = Lens.lens (\Action' {description} -> description) (\s@Action' {} a -> s {description = a} :: Action)

-- | The tags for the action.
action_tags :: Lens.Lens' Action (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
action_tags = Lens.lens (\Action' {tags} -> tags) (\s@Action' {} a -> s {tags = a} :: Action) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Core..:? "parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Action

instance Prelude.NFData Action
