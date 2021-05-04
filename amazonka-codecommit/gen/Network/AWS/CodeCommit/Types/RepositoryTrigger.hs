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
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryTrigger where

import Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a trigger for a repository.
--
-- /See:/ 'newRepositoryTrigger' smart constructor.
data RepositoryTrigger = RepositoryTrigger'
  { -- | Any custom data associated with the trigger to be included in the
    -- information sent to the target of the trigger.
    customData :: Prelude.Maybe Prelude.Text,
    -- | The branches to be included in the trigger configuration. If you specify
    -- an empty array, the trigger applies to all branches.
    --
    -- Although no content is required in the array, you must include the array
    -- itself.
    branches :: Prelude.Maybe [Prelude.Text],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RepositoryTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customData', 'repositoryTrigger_customData' - Any custom data associated with the trigger to be included in the
-- information sent to the target of the trigger.
--
-- 'branches', 'repositoryTrigger_branches' - The branches to be included in the trigger configuration. If you specify
-- an empty array, the trigger applies to all branches.
--
-- Although no content is required in the array, you must include the array
-- itself.
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
    { customData = Prelude.Nothing,
      branches = Prelude.Nothing,
      name = pName_,
      destinationArn = pDestinationArn_,
      events = Prelude.mempty
    }

-- | Any custom data associated with the trigger to be included in the
-- information sent to the target of the trigger.
repositoryTrigger_customData :: Lens.Lens' RepositoryTrigger (Prelude.Maybe Prelude.Text)
repositoryTrigger_customData = Lens.lens (\RepositoryTrigger' {customData} -> customData) (\s@RepositoryTrigger' {} a -> s {customData = a} :: RepositoryTrigger)

-- | The branches to be included in the trigger configuration. If you specify
-- an empty array, the trigger applies to all branches.
--
-- Although no content is required in the array, you must include the array
-- itself.
repositoryTrigger_branches :: Lens.Lens' RepositoryTrigger (Prelude.Maybe [Prelude.Text])
repositoryTrigger_branches = Lens.lens (\RepositoryTrigger' {branches} -> branches) (\s@RepositoryTrigger' {} a -> s {branches = a} :: RepositoryTrigger) Prelude.. Lens.mapping Prelude._Coerce

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
repositoryTrigger_events = Lens.lens (\RepositoryTrigger' {events} -> events) (\s@RepositoryTrigger' {} a -> s {events = a} :: RepositoryTrigger) Prelude.. Prelude._Coerce

instance Prelude.FromJSON RepositoryTrigger where
  parseJSON =
    Prelude.withObject
      "RepositoryTrigger"
      ( \x ->
          RepositoryTrigger'
            Prelude.<$> (x Prelude..:? "customData")
            Prelude.<*> (x Prelude..:? "branches" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "destinationArn")
            Prelude.<*> (x Prelude..:? "events" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable RepositoryTrigger

instance Prelude.NFData RepositoryTrigger

instance Prelude.ToJSON RepositoryTrigger where
  toJSON RepositoryTrigger' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("customData" Prelude..=) Prelude.<$> customData,
            ("branches" Prelude..=) Prelude.<$> branches,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just
              ("destinationArn" Prelude..= destinationArn),
            Prelude.Just ("events" Prelude..= events)
          ]
      )
