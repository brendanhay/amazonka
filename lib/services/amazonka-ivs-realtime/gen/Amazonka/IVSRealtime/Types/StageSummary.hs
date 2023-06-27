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
-- Module      : Amazonka.IVSRealtime.Types.StageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.StageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a stage.
--
-- /See:/ 'newStageSummary' smart constructor.
data StageSummary = StageSummary'
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
-- Create a value of 'StageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeSessionId', 'stageSummary_activeSessionId' - ID of the active session within the stage.
--
-- 'name', 'stageSummary_name' - Stage name.
--
-- 'tags', 'stageSummary_tags' - Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS has no constraints on tags beyond
-- what is documented there.
--
-- 'arn', 'stageSummary_arn' - Stage ARN.
newStageSummary ::
  -- | 'arn'
  Prelude.Text ->
  StageSummary
newStageSummary pArn_ =
  StageSummary'
    { activeSessionId = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      arn = pArn_
    }

-- | ID of the active session within the stage.
stageSummary_activeSessionId :: Lens.Lens' StageSummary (Prelude.Maybe Prelude.Text)
stageSummary_activeSessionId = Lens.lens (\StageSummary' {activeSessionId} -> activeSessionId) (\s@StageSummary' {} a -> s {activeSessionId = a} :: StageSummary)

-- | Stage name.
stageSummary_name :: Lens.Lens' StageSummary (Prelude.Maybe Prelude.Text)
stageSummary_name = Lens.lens (\StageSummary' {name} -> name) (\s@StageSummary' {} a -> s {name = a} :: StageSummary)

-- | Tags attached to the resource. Array of maps, each of the form
-- @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for details, including restrictions that apply to tags and \"Tag naming
-- limits and requirements\"; Amazon IVS has no constraints on tags beyond
-- what is documented there.
stageSummary_tags :: Lens.Lens' StageSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stageSummary_tags = Lens.lens (\StageSummary' {tags} -> tags) (\s@StageSummary' {} a -> s {tags = a} :: StageSummary) Prelude.. Lens.mapping Lens.coerced

-- | Stage ARN.
stageSummary_arn :: Lens.Lens' StageSummary Prelude.Text
stageSummary_arn = Lens.lens (\StageSummary' {arn} -> arn) (\s@StageSummary' {} a -> s {arn = a} :: StageSummary)

instance Data.FromJSON StageSummary where
  parseJSON =
    Data.withObject
      "StageSummary"
      ( \x ->
          StageSummary'
            Prelude.<$> (x Data..:? "activeSessionId")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
      )

instance Prelude.Hashable StageSummary where
  hashWithSalt _salt StageSummary' {..} =
    _salt
      `Prelude.hashWithSalt` activeSessionId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn

instance Prelude.NFData StageSummary where
  rnf StageSummary' {..} =
    Prelude.rnf activeSessionId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
