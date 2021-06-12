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
-- Module      : Network.AWS.MediaLive.Types.OutputLocationRef
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputLocationRef where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Reference to an OutputDestination ID defined in the channel
--
-- /See:/ 'newOutputLocationRef' smart constructor.
data OutputLocationRef = OutputLocationRef'
  { destinationRefId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutputLocationRef' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationRefId', 'outputLocationRef_destinationRefId' - Undocumented member.
newOutputLocationRef ::
  OutputLocationRef
newOutputLocationRef =
  OutputLocationRef' {destinationRefId = Core.Nothing}

-- | Undocumented member.
outputLocationRef_destinationRefId :: Lens.Lens' OutputLocationRef (Core.Maybe Core.Text)
outputLocationRef_destinationRefId = Lens.lens (\OutputLocationRef' {destinationRefId} -> destinationRefId) (\s@OutputLocationRef' {} a -> s {destinationRefId = a} :: OutputLocationRef)

instance Core.FromJSON OutputLocationRef where
  parseJSON =
    Core.withObject
      "OutputLocationRef"
      ( \x ->
          OutputLocationRef'
            Core.<$> (x Core..:? "destinationRefId")
      )

instance Core.Hashable OutputLocationRef

instance Core.NFData OutputLocationRef

instance Core.ToJSON OutputLocationRef where
  toJSON OutputLocationRef' {..} =
    Core.object
      ( Core.catMaybes
          [ ("destinationRefId" Core..=)
              Core.<$> destinationRefId
          ]
      )
