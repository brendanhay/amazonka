{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Nimble.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.GetLaunchProfile
import Amazonka.Nimble.GetStreamingImage
import Amazonka.Nimble.GetStreamingSession
import Amazonka.Nimble.GetStreamingSessionStream
import Amazonka.Nimble.GetStudio
import Amazonka.Nimble.GetStudioComponent
import Amazonka.Nimble.Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.Nimble.GetLaunchProfile' every 5 seconds until a successful state is reached. An error is returned after 150 failed checks.
newLaunchProfileDeleted :: Core.Wait GetLaunchProfile
newLaunchProfileDeleted =
  Core.Wait
    { Core.name = "LaunchProfileDeleted",
      Core.attempts = 150,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( getLaunchProfileResponse_launchProfile
                Prelude.. Lens._Just
                Prelude.. launchProfile_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( getLaunchProfileResponse_launchProfile
                Prelude.. Lens._Just
                Prelude.. launchProfile_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetLaunchProfile' every 5 seconds until a successful state is reached. An error is returned after 150 failed checks.
newLaunchProfileReady :: Core.Wait GetLaunchProfile
newLaunchProfileReady =
  Core.Wait
    { Core.name = "LaunchProfileReady",
      Core.attempts = 150,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "READY"
            Core.AcceptSuccess
            ( getLaunchProfileResponse_launchProfile
                Prelude.. Lens._Just
                Prelude.. launchProfile_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( getLaunchProfileResponse_launchProfile
                Prelude.. Lens._Just
                Prelude.. launchProfile_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( getLaunchProfileResponse_launchProfile
                Prelude.. Lens._Just
                Prelude.. launchProfile_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStreamingImage' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
newStreamingImageDeleted :: Core.Wait GetStreamingImage
newStreamingImageDeleted =
  Core.Wait
    { Core.name = "StreamingImageDeleted",
      Core.attempts = 60,
      Core.delay = 2,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( getStreamingImageResponse_streamingImage
                Prelude.. Lens._Just
                Prelude.. streamingImage_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( getStreamingImageResponse_streamingImage
                Prelude.. Lens._Just
                Prelude.. streamingImage_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStreamingImage' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
newStreamingImageReady :: Core.Wait GetStreamingImage
newStreamingImageReady =
  Core.Wait
    { Core.name = "StreamingImageReady",
      Core.attempts = 60,
      Core.delay = 2,
      Core.acceptors =
        [ Core.matchAll
            "READY"
            Core.AcceptSuccess
            ( getStreamingImageResponse_streamingImage
                Prelude.. Lens._Just
                Prelude.. streamingImage_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( getStreamingImageResponse_streamingImage
                Prelude.. Lens._Just
                Prelude.. streamingImage_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( getStreamingImageResponse_streamingImage
                Prelude.. Lens._Just
                Prelude.. streamingImage_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStreamingSession' every 5 seconds until a successful state is reached. An error is returned after 180 failed checks.
newStreamingSessionDeleted :: Core.Wait GetStreamingSession
newStreamingSessionDeleted =
  Core.Wait
    { Core.name = "StreamingSessionDeleted",
      Core.attempts = 180,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( getStreamingSessionResponse_session
                Prelude.. Lens._Just
                Prelude.. streamingSession_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( getStreamingSessionResponse_session
                Prelude.. Lens._Just
                Prelude.. streamingSession_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStreamingSession' every 10 seconds until a successful state is reached. An error is returned after 180 failed checks.
newStreamingSessionReady :: Core.Wait GetStreamingSession
newStreamingSessionReady =
  Core.Wait
    { Core.name = "StreamingSessionReady",
      Core.attempts = 180,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "READY"
            Core.AcceptSuccess
            ( getStreamingSessionResponse_session
                Prelude.. Lens._Just
                Prelude.. streamingSession_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( getStreamingSessionResponse_session
                Prelude.. Lens._Just
                Prelude.. streamingSession_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "START_FAILED"
            Core.AcceptFailure
            ( getStreamingSessionResponse_session
                Prelude.. Lens._Just
                Prelude.. streamingSession_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStreamingSession' every 5 seconds until a successful state is reached. An error is returned after 180 failed checks.
newStreamingSessionStopped :: Core.Wait GetStreamingSession
newStreamingSessionStopped =
  Core.Wait
    { Core.name = "StreamingSessionStopped",
      Core.attempts = 180,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "STOPPED"
            Core.AcceptSuccess
            ( getStreamingSessionResponse_session
                Prelude.. Lens._Just
                Prelude.. streamingSession_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STOP_FAILED"
            Core.AcceptFailure
            ( getStreamingSessionResponse_session
                Prelude.. Lens._Just
                Prelude.. streamingSession_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStreamingSessionStream' every 5 seconds until a successful state is reached. An error is returned after 30 failed checks.
newStreamingSessionStreamReady :: Core.Wait GetStreamingSessionStream
newStreamingSessionStreamReady =
  Core.Wait
    { Core.name =
        "StreamingSessionStreamReady",
      Core.attempts = 30,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "READY"
            Core.AcceptSuccess
            ( getStreamingSessionStreamResponse_stream
                Prelude.. Lens._Just
                Prelude.. streamingSessionStream_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( getStreamingSessionStreamResponse_stream
                Prelude.. Lens._Just
                Prelude.. streamingSessionStream_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStudioComponent' every 1 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStudioComponentDeleted :: Core.Wait GetStudioComponent
newStudioComponentDeleted =
  Core.Wait
    { Core.name = "StudioComponentDeleted",
      Core.attempts = 120,
      Core.delay = 1,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( getStudioComponentResponse_studioComponent
                Prelude.. Lens._Just
                Prelude.. studioComponent_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( getStudioComponentResponse_studioComponent
                Prelude.. Lens._Just
                Prelude.. studioComponent_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStudioComponent' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
newStudioComponentReady :: Core.Wait GetStudioComponent
newStudioComponentReady =
  Core.Wait
    { Core.name = "StudioComponentReady",
      Core.attempts = 60,
      Core.delay = 2,
      Core.acceptors =
        [ Core.matchAll
            "READY"
            Core.AcceptSuccess
            ( getStudioComponentResponse_studioComponent
                Prelude.. Lens._Just
                Prelude.. studioComponent_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( getStudioComponentResponse_studioComponent
                Prelude.. Lens._Just
                Prelude.. studioComponent_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( getStudioComponentResponse_studioComponent
                Prelude.. Lens._Just
                Prelude.. studioComponent_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStudio' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
newStudioDeleted :: Core.Wait GetStudio
newStudioDeleted =
  Core.Wait
    { Core.name = "StudioDeleted",
      Core.attempts = 60,
      Core.delay = 2,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( getStudioResponse_studio
                Prelude.. studio_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DELETE_FAILED"
            Core.AcceptFailure
            ( getStudioResponse_studio
                Prelude.. studio_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Nimble.GetStudio' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
newStudioReady :: Core.Wait GetStudio
newStudioReady =
  Core.Wait
    { Core.name = "StudioReady",
      Core.attempts = 60,
      Core.delay = 2,
      Core.acceptors =
        [ Core.matchAll
            "READY"
            Core.AcceptSuccess
            ( getStudioResponse_studio
                Prelude.. studio_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( getStudioResponse_studio
                Prelude.. studio_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( getStudioResponse_studio
                Prelude.. studio_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
