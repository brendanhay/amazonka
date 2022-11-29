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
-- Module      : Amazonka.SSMIncidents.Types.EventReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.EventReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An item referenced in a @TimelineEvent@ that is involved in or somehow
-- associated with an incident. You can specify an Amazon Resource Name
-- (ARN) for an Amazon Web Services resource or a @RelatedItem@ ID.
--
-- /See:/ 'newEventReference' smart constructor.
data EventReference = EventReference'
  { -- | The ID of a @RelatedItem@ referenced in a @TimelineEvent@.
    relatedItemId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Amazon Web Services resource
    -- referenced in a @TimelineEvent@.
    resource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relatedItemId', 'eventReference_relatedItemId' - The ID of a @RelatedItem@ referenced in a @TimelineEvent@.
--
-- 'resource', 'eventReference_resource' - The Amazon Resource Name (ARN) of an Amazon Web Services resource
-- referenced in a @TimelineEvent@.
newEventReference ::
  EventReference
newEventReference =
  EventReference'
    { relatedItemId = Prelude.Nothing,
      resource = Prelude.Nothing
    }

-- | The ID of a @RelatedItem@ referenced in a @TimelineEvent@.
eventReference_relatedItemId :: Lens.Lens' EventReference (Prelude.Maybe Prelude.Text)
eventReference_relatedItemId = Lens.lens (\EventReference' {relatedItemId} -> relatedItemId) (\s@EventReference' {} a -> s {relatedItemId = a} :: EventReference)

-- | The Amazon Resource Name (ARN) of an Amazon Web Services resource
-- referenced in a @TimelineEvent@.
eventReference_resource :: Lens.Lens' EventReference (Prelude.Maybe Prelude.Text)
eventReference_resource = Lens.lens (\EventReference' {resource} -> resource) (\s@EventReference' {} a -> s {resource = a} :: EventReference)

instance Core.FromJSON EventReference where
  parseJSON =
    Core.withObject
      "EventReference"
      ( \x ->
          EventReference'
            Prelude.<$> (x Core..:? "relatedItemId")
            Prelude.<*> (x Core..:? "resource")
      )

instance Prelude.Hashable EventReference where
  hashWithSalt _salt EventReference' {..} =
    _salt `Prelude.hashWithSalt` relatedItemId
      `Prelude.hashWithSalt` resource

instance Prelude.NFData EventReference where
  rnf EventReference' {..} =
    Prelude.rnf relatedItemId
      `Prelude.seq` Prelude.rnf resource

instance Core.ToJSON EventReference where
  toJSON EventReference' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("relatedItemId" Core..=) Prelude.<$> relatedItemId,
            ("resource" Core..=) Prelude.<$> resource
          ]
      )
