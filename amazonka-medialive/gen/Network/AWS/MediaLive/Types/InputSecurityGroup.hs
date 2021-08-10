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
-- Module      : Network.AWS.MediaLive.Types.InputSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSecurityGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputSecurityGroupState
import Network.AWS.MediaLive.Types.InputWhitelistRule
import qualified Network.AWS.Prelude as Prelude

-- | An Input Security Group
--
-- /See:/ 'newInputSecurityGroup' smart constructor.
data InputSecurityGroup = InputSecurityGroup'
  { -- | Unique ARN of Input Security Group
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Id of the Input Security Group
    id :: Prelude.Maybe Prelude.Text,
    -- | The current state of the Input Security Group.
    state :: Prelude.Maybe InputSecurityGroupState,
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Whitelist rules and their sync status
    whitelistRules :: Prelude.Maybe [InputWhitelistRule],
    -- | The list of inputs currently using this Input Security Group.
    inputs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'inputSecurityGroup_arn' - Unique ARN of Input Security Group
--
-- 'id', 'inputSecurityGroup_id' - The Id of the Input Security Group
--
-- 'state', 'inputSecurityGroup_state' - The current state of the Input Security Group.
--
-- 'tags', 'inputSecurityGroup_tags' - A collection of key-value pairs.
--
-- 'whitelistRules', 'inputSecurityGroup_whitelistRules' - Whitelist rules and their sync status
--
-- 'inputs', 'inputSecurityGroup_inputs' - The list of inputs currently using this Input Security Group.
newInputSecurityGroup ::
  InputSecurityGroup
newInputSecurityGroup =
  InputSecurityGroup'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      whitelistRules = Prelude.Nothing,
      inputs = Prelude.Nothing
    }

-- | Unique ARN of Input Security Group
inputSecurityGroup_arn :: Lens.Lens' InputSecurityGroup (Prelude.Maybe Prelude.Text)
inputSecurityGroup_arn = Lens.lens (\InputSecurityGroup' {arn} -> arn) (\s@InputSecurityGroup' {} a -> s {arn = a} :: InputSecurityGroup)

-- | The Id of the Input Security Group
inputSecurityGroup_id :: Lens.Lens' InputSecurityGroup (Prelude.Maybe Prelude.Text)
inputSecurityGroup_id = Lens.lens (\InputSecurityGroup' {id} -> id) (\s@InputSecurityGroup' {} a -> s {id = a} :: InputSecurityGroup)

-- | The current state of the Input Security Group.
inputSecurityGroup_state :: Lens.Lens' InputSecurityGroup (Prelude.Maybe InputSecurityGroupState)
inputSecurityGroup_state = Lens.lens (\InputSecurityGroup' {state} -> state) (\s@InputSecurityGroup' {} a -> s {state = a} :: InputSecurityGroup)

-- | A collection of key-value pairs.
inputSecurityGroup_tags :: Lens.Lens' InputSecurityGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
inputSecurityGroup_tags = Lens.lens (\InputSecurityGroup' {tags} -> tags) (\s@InputSecurityGroup' {} a -> s {tags = a} :: InputSecurityGroup) Prelude.. Lens.mapping Lens._Coerce

-- | Whitelist rules and their sync status
inputSecurityGroup_whitelistRules :: Lens.Lens' InputSecurityGroup (Prelude.Maybe [InputWhitelistRule])
inputSecurityGroup_whitelistRules = Lens.lens (\InputSecurityGroup' {whitelistRules} -> whitelistRules) (\s@InputSecurityGroup' {} a -> s {whitelistRules = a} :: InputSecurityGroup) Prelude.. Lens.mapping Lens._Coerce

-- | The list of inputs currently using this Input Security Group.
inputSecurityGroup_inputs :: Lens.Lens' InputSecurityGroup (Prelude.Maybe [Prelude.Text])
inputSecurityGroup_inputs = Lens.lens (\InputSecurityGroup' {inputs} -> inputs) (\s@InputSecurityGroup' {} a -> s {inputs = a} :: InputSecurityGroup) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON InputSecurityGroup where
  parseJSON =
    Core.withObject
      "InputSecurityGroup"
      ( \x ->
          InputSecurityGroup'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "whitelistRules" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "inputs" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable InputSecurityGroup

instance Prelude.NFData InputSecurityGroup
