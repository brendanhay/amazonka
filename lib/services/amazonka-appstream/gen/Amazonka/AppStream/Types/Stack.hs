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
-- Module      : Amazonka.AppStream.Types.Stack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.Stack where

import Amazonka.AppStream.Types.AccessEndpoint
import Amazonka.AppStream.Types.ApplicationSettingsResponse
import Amazonka.AppStream.Types.StackError
import Amazonka.AppStream.Types.StorageConnector
import Amazonka.AppStream.Types.StreamingExperienceSettings
import Amazonka.AppStream.Types.UserSetting
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a stack.
--
-- /See:/ 'newStack' smart constructor.
data Stack = Stack'
  { -- | The list of virtual private cloud (VPC) interface endpoint objects.
    -- Users of the stack can connect to AppStream 2.0 only through the
    -- specified endpoints.
    accessEndpoints :: Prelude.Maybe (Prelude.NonEmpty AccessEndpoint),
    -- | The persistent application settings for users of the stack.
    applicationSettings :: Prelude.Maybe ApplicationSettingsResponse,
    -- | The ARN of the stack.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the stack was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The stack name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an
    -- iframe. You must approve the domains that you want to host embedded
    -- AppStream 2.0 streaming sessions.
    embedHostDomains :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The URL that users are redirected to after they click the Send Feedback
    -- link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Prelude.Maybe Prelude.Text,
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Prelude.Maybe Prelude.Text,
    -- | The errors for the stack.
    stackErrors :: Prelude.Maybe [StackError],
    -- | The storage connectors to enable.
    storageConnectors :: Prelude.Maybe [StorageConnector],
    -- | The streaming protocol you want your stack to prefer. This can be UDP or
    -- TCP. Currently, UDP is only supported in the Windows native client.
    streamingExperienceSettings :: Prelude.Maybe StreamingExperienceSettings,
    -- | The actions that are enabled or disabled for users during their
    -- streaming sessions. By default these actions are enabled.
    userSettings :: Prelude.Maybe (Prelude.NonEmpty UserSetting),
    -- | The name of the stack.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Stack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessEndpoints', 'stack_accessEndpoints' - The list of virtual private cloud (VPC) interface endpoint objects.
-- Users of the stack can connect to AppStream 2.0 only through the
-- specified endpoints.
--
-- 'applicationSettings', 'stack_applicationSettings' - The persistent application settings for users of the stack.
--
-- 'arn', 'stack_arn' - The ARN of the stack.
--
-- 'createdTime', 'stack_createdTime' - The time the stack was created.
--
-- 'description', 'stack_description' - The description to display.
--
-- 'displayName', 'stack_displayName' - The stack name to display.
--
-- 'embedHostDomains', 'stack_embedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
--
-- 'feedbackURL', 'stack_feedbackURL' - The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
--
-- 'redirectURL', 'stack_redirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- 'stackErrors', 'stack_stackErrors' - The errors for the stack.
--
-- 'storageConnectors', 'stack_storageConnectors' - The storage connectors to enable.
--
-- 'streamingExperienceSettings', 'stack_streamingExperienceSettings' - The streaming protocol you want your stack to prefer. This can be UDP or
-- TCP. Currently, UDP is only supported in the Windows native client.
--
-- 'userSettings', 'stack_userSettings' - The actions that are enabled or disabled for users during their
-- streaming sessions. By default these actions are enabled.
--
-- 'name', 'stack_name' - The name of the stack.
newStack ::
  -- | 'name'
  Prelude.Text ->
  Stack
newStack pName_ =
  Stack'
    { accessEndpoints = Prelude.Nothing,
      applicationSettings = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      embedHostDomains = Prelude.Nothing,
      feedbackURL = Prelude.Nothing,
      redirectURL = Prelude.Nothing,
      stackErrors = Prelude.Nothing,
      storageConnectors = Prelude.Nothing,
      streamingExperienceSettings = Prelude.Nothing,
      userSettings = Prelude.Nothing,
      name = pName_
    }

-- | The list of virtual private cloud (VPC) interface endpoint objects.
-- Users of the stack can connect to AppStream 2.0 only through the
-- specified endpoints.
stack_accessEndpoints :: Lens.Lens' Stack (Prelude.Maybe (Prelude.NonEmpty AccessEndpoint))
stack_accessEndpoints = Lens.lens (\Stack' {accessEndpoints} -> accessEndpoints) (\s@Stack' {} a -> s {accessEndpoints = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | The persistent application settings for users of the stack.
stack_applicationSettings :: Lens.Lens' Stack (Prelude.Maybe ApplicationSettingsResponse)
stack_applicationSettings = Lens.lens (\Stack' {applicationSettings} -> applicationSettings) (\s@Stack' {} a -> s {applicationSettings = a} :: Stack)

-- | The ARN of the stack.
stack_arn :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_arn = Lens.lens (\Stack' {arn} -> arn) (\s@Stack' {} a -> s {arn = a} :: Stack)

-- | The time the stack was created.
stack_createdTime :: Lens.Lens' Stack (Prelude.Maybe Prelude.UTCTime)
stack_createdTime = Lens.lens (\Stack' {createdTime} -> createdTime) (\s@Stack' {} a -> s {createdTime = a} :: Stack) Prelude.. Lens.mapping Data._Time

-- | The description to display.
stack_description :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_description = Lens.lens (\Stack' {description} -> description) (\s@Stack' {} a -> s {description = a} :: Stack)

-- | The stack name to display.
stack_displayName :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_displayName = Lens.lens (\Stack' {displayName} -> displayName) (\s@Stack' {} a -> s {displayName = a} :: Stack)

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
stack_embedHostDomains :: Lens.Lens' Stack (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
stack_embedHostDomains = Lens.lens (\Stack' {embedHostDomains} -> embedHostDomains) (\s@Stack' {} a -> s {embedHostDomains = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
stack_feedbackURL :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_feedbackURL = Lens.lens (\Stack' {feedbackURL} -> feedbackURL) (\s@Stack' {} a -> s {feedbackURL = a} :: Stack)

-- | The URL that users are redirected to after their streaming session ends.
stack_redirectURL :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_redirectURL = Lens.lens (\Stack' {redirectURL} -> redirectURL) (\s@Stack' {} a -> s {redirectURL = a} :: Stack)

-- | The errors for the stack.
stack_stackErrors :: Lens.Lens' Stack (Prelude.Maybe [StackError])
stack_stackErrors = Lens.lens (\Stack' {stackErrors} -> stackErrors) (\s@Stack' {} a -> s {stackErrors = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | The storage connectors to enable.
stack_storageConnectors :: Lens.Lens' Stack (Prelude.Maybe [StorageConnector])
stack_storageConnectors = Lens.lens (\Stack' {storageConnectors} -> storageConnectors) (\s@Stack' {} a -> s {storageConnectors = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | The streaming protocol you want your stack to prefer. This can be UDP or
-- TCP. Currently, UDP is only supported in the Windows native client.
stack_streamingExperienceSettings :: Lens.Lens' Stack (Prelude.Maybe StreamingExperienceSettings)
stack_streamingExperienceSettings = Lens.lens (\Stack' {streamingExperienceSettings} -> streamingExperienceSettings) (\s@Stack' {} a -> s {streamingExperienceSettings = a} :: Stack)

-- | The actions that are enabled or disabled for users during their
-- streaming sessions. By default these actions are enabled.
stack_userSettings :: Lens.Lens' Stack (Prelude.Maybe (Prelude.NonEmpty UserSetting))
stack_userSettings = Lens.lens (\Stack' {userSettings} -> userSettings) (\s@Stack' {} a -> s {userSettings = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stack.
stack_name :: Lens.Lens' Stack Prelude.Text
stack_name = Lens.lens (\Stack' {name} -> name) (\s@Stack' {} a -> s {name = a} :: Stack)

instance Data.FromJSON Stack where
  parseJSON =
    Data.withObject
      "Stack"
      ( \x ->
          Stack'
            Prelude.<$> (x Data..:? "AccessEndpoints")
            Prelude.<*> (x Data..:? "ApplicationSettings")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "EmbedHostDomains")
            Prelude.<*> (x Data..:? "FeedbackURL")
            Prelude.<*> (x Data..:? "RedirectURL")
            Prelude.<*> (x Data..:? "StackErrors" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "StorageConnectors"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StreamingExperienceSettings")
            Prelude.<*> (x Data..:? "UserSettings")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Stack where
  hashWithSalt _salt Stack' {..} =
    _salt
      `Prelude.hashWithSalt` accessEndpoints
      `Prelude.hashWithSalt` applicationSettings
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` embedHostDomains
      `Prelude.hashWithSalt` feedbackURL
      `Prelude.hashWithSalt` redirectURL
      `Prelude.hashWithSalt` stackErrors
      `Prelude.hashWithSalt` storageConnectors
      `Prelude.hashWithSalt` streamingExperienceSettings
      `Prelude.hashWithSalt` userSettings
      `Prelude.hashWithSalt` name

instance Prelude.NFData Stack where
  rnf Stack' {..} =
    Prelude.rnf accessEndpoints
      `Prelude.seq` Prelude.rnf applicationSettings
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf embedHostDomains
      `Prelude.seq` Prelude.rnf feedbackURL
      `Prelude.seq` Prelude.rnf redirectURL
      `Prelude.seq` Prelude.rnf stackErrors
      `Prelude.seq` Prelude.rnf storageConnectors
      `Prelude.seq` Prelude.rnf streamingExperienceSettings
      `Prelude.seq` Prelude.rnf userSettings
      `Prelude.seq` Prelude.rnf name
