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
-- Module      : Amazonka.CodeCommit.Types.RepositoryTrigger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.RepositoryTrigger where

import Amazonka.CodeCommit.Types.RepositoryTriggerEventEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a trigger for a repository.
--
-- /See:/ 'newRepositoryTrigger' smart constructor.
data RepositoryTrigger = RepositoryTrigger'
  { -- | The branches to be included in the trigger configuration. If you specify
    -- an empty array, the trigger applies to all branches.
    --
    -- Although no content is required in the array, you must include the array
    -- itself.
    branches :: Prelude.Maybe [Prelude.Text],
    -- | Any custom data associated with the trigger to be included in the
    -- information sent to the target of the trigger.
    customData :: Prelude.Maybe Prelude.Text,
    -- | The name of the trigger.
    name :: Prelude.Text,
    -- | The ARN of the resource that is the target for a trigger (for example,
    -- the ARN of a topic in Amazon SNS).
    destinationArn :: Prelude.Text,
    -- | The repository events that cause the trigger to run actions in another
    -- service, such as sending a notification through Amazon SNS.
    --
    -- The valid value \"all\" cannot be used with any other values.
    events :: [RepositoryTriggerEventEnum]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branches', 'repositoryTrigger_branches' - The branches to be included in the trigger configuration. If you specify
-- an empty array, the trigger applies to all branches.
--
-- Although no content is required in the array, you must include the array
-- itself.
--
-- 'customData', 'repositoryTrigger_customData' - Any custom data associated with the trigger to be included in the
-- information sent to the target of the trigger.
--
-- 'name', 'repositoryTrigger_name' - The name of the trigger.
--
-- 'destinationArn', 'repositoryTrigger_destinationArn' - The ARN of the resource that is the target for a trigger (for example,
-- the ARN of a topic in Amazon SNS).
--
-- 'events', 'repositoryTrigger_events' - The repository events that cause the trigger to run actions in another
-- service, such as sending a notification through Amazon SNS.
--
-- The valid value \"all\" cannot be used with any other values.
newRepositoryTrigger ::
  -- | 'name'
  Prelude.Text ->
  -- | 'destinationArn'
  Prelude.Text ->
  RepositoryTrigger
newRepositoryTrigger pName_ pDestinationArn_ =
  RepositoryTrigger'
    { branches = Prelude.Nothing,
      customData = Prelude.Nothing,
      name = pName_,
      destinationArn = pDestinationArn_,
      events = Prelude.mempty
    }

-- | The branches to be included in the trigger configuration. If you specify
-- an empty array, the trigger applies to all branches.
--
-- Although no content is required in the array, you must include the array
-- itself.
repositoryTrigger_branches :: Lens.Lens' RepositoryTrigger (Prelude.Maybe [Prelude.Text])
repositoryTrigger_branches = Lens.lens (\RepositoryTrigger' {branches} -> branches) (\s@RepositoryTrigger' {} a -> s {branches = a} :: RepositoryTrigger) Prelude.. Lens.mapping Lens.coerced

-- | Any custom data associated with the trigger to be included in the
-- information sent to the target of the trigger.
repositoryTrigger_customData :: Lens.Lens' RepositoryTrigger (Prelude.Maybe Prelude.Text)
repositoryTrigger_customData = Lens.lens (\RepositoryTrigger' {customData} -> customData) (\s@RepositoryTrigger' {} a -> s {customData = a} :: RepositoryTrigger)

-- | The name of the trigger.
repositoryTrigger_name :: Lens.Lens' RepositoryTrigger Prelude.Text
repositoryTrigger_name = Lens.lens (\RepositoryTrigger' {name} -> name) (\s@RepositoryTrigger' {} a -> s {name = a} :: RepositoryTrigger)

-- | The ARN of the resource that is the target for a trigger (for example,
-- the ARN of a topic in Amazon SNS).
repositoryTrigger_destinationArn :: Lens.Lens' RepositoryTrigger Prelude.Text
repositoryTrigger_destinationArn = Lens.lens (\RepositoryTrigger' {destinationArn} -> destinationArn) (\s@RepositoryTrigger' {} a -> s {destinationArn = a} :: RepositoryTrigger)

-- | The repository events that cause the trigger to run actions in another
-- service, such as sending a notification through Amazon SNS.
--
-- The valid value \"all\" cannot be used with any other values.
repositoryTrigger_events :: Lens.Lens' RepositoryTrigger [RepositoryTriggerEventEnum]
repositoryTrigger_events = Lens.lens (\RepositoryTrigger' {events} -> events) (\s@RepositoryTrigger' {} a -> s {events = a} :: RepositoryTrigger) Prelude.. Lens.coerced

instance Data.FromJSON RepositoryTrigger where
  parseJSON =
    Data.withObject
      "RepositoryTrigger"
      ( \x ->
          RepositoryTrigger'
            Prelude.<$> (x Data..:? "branches" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "customData")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "destinationArn")
            Prelude.<*> (x Data..:? "events" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RepositoryTrigger where
  hashWithSalt _salt RepositoryTrigger' {..} =
    _salt
      `Prelude.hashWithSalt` branches
      `Prelude.hashWithSalt` customData
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` events

instance Prelude.NFData RepositoryTrigger where
  rnf RepositoryTrigger' {..} =
    Prelude.rnf branches
      `Prelude.seq` Prelude.rnf customData
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf events

instance Data.ToJSON RepositoryTrigger where
  toJSON RepositoryTrigger' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("branches" Data..=) Prelude.<$> branches,
            ("customData" Data..=) Prelude.<$> customData,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("destinationArn" Data..= destinationArn),
            Prelude.Just ("events" Data..= events)
          ]
      )
