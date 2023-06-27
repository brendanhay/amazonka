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
-- Module      : Amazonka.IVSRealtime.Types.Stage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.Stage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object specifying a stage.
--
-- /See:/ 'newStage' smart constructor.
data Stage = Stage'
  { -- | ID of the active session within the stage.
    activeSessionId :: Prelude.Maybe Prelude.Text,
    -- | Stage name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Tags attached to the resource. Array of maps, each of the form
    -- @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- for details, including restrictions that apply to tags and \"Tag naming
    -- limits and requirements\"; Amazon IVS has no constraints on tags beyond
    -- what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Stage ARN.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Stage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeSessionId', 'stage_activeSessionId' - ID of the active session within the stage.
--
-- 'name', 'stage_name' - Stage name.
--
-- 'tags', 'stage_tags' - Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS has no constraints on tags beyond
-- what is documented there.
--
-- 'arn', 'stage_arn' - Stage ARN.
newStage ::
  -- | 'arn'
  Prelude.Text ->
  Stage
newStage pArn_ =
  Stage'
    { activeSessionId = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      arn = pArn_
    }

-- | ID of the active session within the stage.
stage_activeSessionId :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_activeSessionId = Lens.lens (\Stage' {activeSessionId} -> activeSessionId) (\s@Stage' {} a -> s {activeSessionId = a} :: Stage)

-- | Stage name.
stage_name :: Lens.Lens' Stage (Prelude.Maybe Prelude.Text)
stage_name = Lens.lens (\Stage' {name} -> name) (\s@Stage' {} a -> s {name = a} :: Stage)

-- | Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS has no constraints on tags beyond
-- what is documented there.
stage_tags :: Lens.Lens' Stage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stage_tags = Lens.lens (\Stage' {tags} -> tags) (\s@Stage' {} a -> s {tags = a} :: Stage) Prelude.. Lens.mapping Lens.coerced

-- | Stage ARN.
stage_arn :: Lens.Lens' Stage Prelude.Text
stage_arn = Lens.lens (\Stage' {arn} -> arn) (\s@Stage' {} a -> s {arn = a} :: Stage)

instance Data.FromJSON Stage where
  parseJSON =
    Data.withObject
      "Stage"
      ( \x ->
          Stage'
            Prelude.<$> (x Data..:? "activeSessionId")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
      )

instance Prelude.Hashable Stage where
  hashWithSalt _salt Stage' {..} =
    _salt
      `Prelude.hashWithSalt` activeSessionId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn

instance Prelude.NFData Stage where
  rnf Stage' {..} =
    Prelude.rnf activeSessionId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
