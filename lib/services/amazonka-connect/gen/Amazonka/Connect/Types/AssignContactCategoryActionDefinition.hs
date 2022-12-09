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
-- Module      : Amazonka.Connect.Types.AssignContactCategoryActionDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.AssignContactCategoryActionDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This action must be set if @TriggerEventSource@ is one of the following
-- values: @OnPostCallAnalysisAvailable@ |
-- @OnRealTimeCallAnalysisAvailable@ | @OnPostChatAnalysisAvailable@.
-- Contact is categorized using the rule name.
--
-- @RuleName@ is used as @ContactCategory@.
--
-- /See:/ 'newAssignContactCategoryActionDefinition' smart constructor.
data AssignContactCategoryActionDefinition = AssignContactCategoryActionDefinition'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssignContactCategoryActionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssignContactCategoryActionDefinition ::
  AssignContactCategoryActionDefinition
newAssignContactCategoryActionDefinition =
  AssignContactCategoryActionDefinition'

instance
  Data.FromJSON
    AssignContactCategoryActionDefinition
  where
  parseJSON =
    Data.withObject
      "AssignContactCategoryActionDefinition"
      ( \x ->
          Prelude.pure AssignContactCategoryActionDefinition'
      )

instance
  Prelude.Hashable
    AssignContactCategoryActionDefinition
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    AssignContactCategoryActionDefinition
  where
  rnf _ = ()

instance
  Data.ToJSON
    AssignContactCategoryActionDefinition
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
