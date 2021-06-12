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
-- Module      : Network.AWS.AlexaBusiness.Types.ConferenceProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferenceProvider where

import Network.AWS.AlexaBusiness.Types.ConferenceProviderType
import Network.AWS.AlexaBusiness.Types.IPDialIn
import Network.AWS.AlexaBusiness.Types.MeetingSetting
import Network.AWS.AlexaBusiness.Types.PSTNDialIn
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An entity that provides a conferencing solution. Alexa for Business acts
-- as the voice interface and mediator that connects users to their
-- preferred conference provider. Examples of conference providers include
-- Amazon Chime, Zoom, Cisco, and Polycom.
--
-- /See:/ 'newConferenceProvider' smart constructor.
data ConferenceProvider = ConferenceProvider'
  { -- | The meeting settings for the conference provider.
    meetingSetting :: Core.Maybe MeetingSetting,
    -- | The IP endpoint and protocol for calling.
    iPDialIn :: Core.Maybe IPDialIn,
    -- | The ARN of the newly created conference provider.
    arn :: Core.Maybe Core.Text,
    -- | The name of the conference provider.
    name :: Core.Maybe Core.Text,
    -- | The information for PSTN conferencing.
    pSTNDialIn :: Core.Maybe PSTNDialIn,
    -- | The type of conference providers.
    type' :: Core.Maybe ConferenceProviderType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConferenceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingSetting', 'conferenceProvider_meetingSetting' - The meeting settings for the conference provider.
--
-- 'iPDialIn', 'conferenceProvider_iPDialIn' - The IP endpoint and protocol for calling.
--
-- 'arn', 'conferenceProvider_arn' - The ARN of the newly created conference provider.
--
-- 'name', 'conferenceProvider_name' - The name of the conference provider.
--
-- 'pSTNDialIn', 'conferenceProvider_pSTNDialIn' - The information for PSTN conferencing.
--
-- 'type'', 'conferenceProvider_type' - The type of conference providers.
newConferenceProvider ::
  ConferenceProvider
newConferenceProvider =
  ConferenceProvider'
    { meetingSetting = Core.Nothing,
      iPDialIn = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      pSTNDialIn = Core.Nothing,
      type' = Core.Nothing
    }

-- | The meeting settings for the conference provider.
conferenceProvider_meetingSetting :: Lens.Lens' ConferenceProvider (Core.Maybe MeetingSetting)
conferenceProvider_meetingSetting = Lens.lens (\ConferenceProvider' {meetingSetting} -> meetingSetting) (\s@ConferenceProvider' {} a -> s {meetingSetting = a} :: ConferenceProvider)

-- | The IP endpoint and protocol for calling.
conferenceProvider_iPDialIn :: Lens.Lens' ConferenceProvider (Core.Maybe IPDialIn)
conferenceProvider_iPDialIn = Lens.lens (\ConferenceProvider' {iPDialIn} -> iPDialIn) (\s@ConferenceProvider' {} a -> s {iPDialIn = a} :: ConferenceProvider)

-- | The ARN of the newly created conference provider.
conferenceProvider_arn :: Lens.Lens' ConferenceProvider (Core.Maybe Core.Text)
conferenceProvider_arn = Lens.lens (\ConferenceProvider' {arn} -> arn) (\s@ConferenceProvider' {} a -> s {arn = a} :: ConferenceProvider)

-- | The name of the conference provider.
conferenceProvider_name :: Lens.Lens' ConferenceProvider (Core.Maybe Core.Text)
conferenceProvider_name = Lens.lens (\ConferenceProvider' {name} -> name) (\s@ConferenceProvider' {} a -> s {name = a} :: ConferenceProvider)

-- | The information for PSTN conferencing.
conferenceProvider_pSTNDialIn :: Lens.Lens' ConferenceProvider (Core.Maybe PSTNDialIn)
conferenceProvider_pSTNDialIn = Lens.lens (\ConferenceProvider' {pSTNDialIn} -> pSTNDialIn) (\s@ConferenceProvider' {} a -> s {pSTNDialIn = a} :: ConferenceProvider)

-- | The type of conference providers.
conferenceProvider_type :: Lens.Lens' ConferenceProvider (Core.Maybe ConferenceProviderType)
conferenceProvider_type = Lens.lens (\ConferenceProvider' {type'} -> type') (\s@ConferenceProvider' {} a -> s {type' = a} :: ConferenceProvider)

instance Core.FromJSON ConferenceProvider where
  parseJSON =
    Core.withObject
      "ConferenceProvider"
      ( \x ->
          ConferenceProvider'
            Core.<$> (x Core..:? "MeetingSetting")
            Core.<*> (x Core..:? "IPDialIn")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "PSTNDialIn")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable ConferenceProvider

instance Core.NFData ConferenceProvider
