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
-- Module      : Network.AWS.AppStream.Types.Stack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Stack where

import Network.AWS.AppStream.Types.AccessEndpoint
import Network.AWS.AppStream.Types.ApplicationSettingsResponse
import Network.AWS.AppStream.Types.StackError
import Network.AWS.AppStream.Types.StorageConnector
import Network.AWS.AppStream.Types.UserSetting
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a stack.
--
-- /See:/ 'newStack' smart constructor.
data Stack = Stack'
  { -- | The actions that are enabled or disabled for users during their
    -- streaming sessions. By default these actions are enabled.
    userSettings :: Prelude.Maybe (Prelude.NonEmpty UserSetting),
    -- | The list of virtual private cloud (VPC) interface endpoint objects.
    -- Users of the stack can connect to AppStream 2.0 only through the
    -- specified endpoints.
    accessEndpoints :: Prelude.Maybe (Prelude.NonEmpty AccessEndpoint),
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the stack.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the stack was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The persistent application settings for users of the stack.
    applicationSettings :: Prelude.Maybe ApplicationSettingsResponse,
    -- | The storage connectors to enable.
    storageConnectors :: Prelude.Maybe [StorageConnector],
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an
    -- iframe. You must approve the domains that you want to host embedded
    -- AppStream 2.0 streaming sessions.
    embedHostDomains :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The errors for the stack.
    stackErrors :: Prelude.Maybe [StackError],
    -- | The stack name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The URL that users are redirected to after they click the Send Feedback
    -- link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Prelude.Maybe Prelude.Text,
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
-- 'userSettings', 'stack_userSettings' - The actions that are enabled or disabled for users during their
-- streaming sessions. By default these actions are enabled.
--
-- 'accessEndpoints', 'stack_accessEndpoints' - The list of virtual private cloud (VPC) interface endpoint objects.
-- Users of the stack can connect to AppStream 2.0 only through the
-- specified endpoints.
--
-- 'redirectURL', 'stack_redirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- 'arn', 'stack_arn' - The ARN of the stack.
--
-- 'createdTime', 'stack_createdTime' - The time the stack was created.
--
-- 'applicationSettings', 'stack_applicationSettings' - The persistent application settings for users of the stack.
--
-- 'storageConnectors', 'stack_storageConnectors' - The storage connectors to enable.
--
-- 'description', 'stack_description' - The description to display.
--
-- 'embedHostDomains', 'stack_embedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
--
-- 'stackErrors', 'stack_stackErrors' - The errors for the stack.
--
-- 'displayName', 'stack_displayName' - The stack name to display.
--
-- 'feedbackURL', 'stack_feedbackURL' - The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
--
-- 'name', 'stack_name' - The name of the stack.
newStack ::
  -- | 'name'
  Prelude.Text ->
  Stack
newStack pName_ =
  Stack'
    { userSettings = Prelude.Nothing,
      accessEndpoints = Prelude.Nothing,
      redirectURL = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      applicationSettings = Prelude.Nothing,
      storageConnectors = Prelude.Nothing,
      description = Prelude.Nothing,
      embedHostDomains = Prelude.Nothing,
      stackErrors = Prelude.Nothing,
      displayName = Prelude.Nothing,
      feedbackURL = Prelude.Nothing,
      name = pName_
    }

-- | The actions that are enabled or disabled for users during their
-- streaming sessions. By default these actions are enabled.
stack_userSettings :: Lens.Lens' Stack (Prelude.Maybe (Prelude.NonEmpty UserSetting))
stack_userSettings = Lens.lens (\Stack' {userSettings} -> userSettings) (\s@Stack' {} a -> s {userSettings = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | The list of virtual private cloud (VPC) interface endpoint objects.
-- Users of the stack can connect to AppStream 2.0 only through the
-- specified endpoints.
stack_accessEndpoints :: Lens.Lens' Stack (Prelude.Maybe (Prelude.NonEmpty AccessEndpoint))
stack_accessEndpoints = Lens.lens (\Stack' {accessEndpoints} -> accessEndpoints) (\s@Stack' {} a -> s {accessEndpoints = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | The URL that users are redirected to after their streaming session ends.
stack_redirectURL :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_redirectURL = Lens.lens (\Stack' {redirectURL} -> redirectURL) (\s@Stack' {} a -> s {redirectURL = a} :: Stack)

-- | The ARN of the stack.
stack_arn :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_arn = Lens.lens (\Stack' {arn} -> arn) (\s@Stack' {} a -> s {arn = a} :: Stack)

-- | The time the stack was created.
stack_createdTime :: Lens.Lens' Stack (Prelude.Maybe Prelude.UTCTime)
stack_createdTime = Lens.lens (\Stack' {createdTime} -> createdTime) (\s@Stack' {} a -> s {createdTime = a} :: Stack) Prelude.. Lens.mapping Core._Time

-- | The persistent application settings for users of the stack.
stack_applicationSettings :: Lens.Lens' Stack (Prelude.Maybe ApplicationSettingsResponse)
stack_applicationSettings = Lens.lens (\Stack' {applicationSettings} -> applicationSettings) (\s@Stack' {} a -> s {applicationSettings = a} :: Stack)

-- | The storage connectors to enable.
stack_storageConnectors :: Lens.Lens' Stack (Prelude.Maybe [StorageConnector])
stack_storageConnectors = Lens.lens (\Stack' {storageConnectors} -> storageConnectors) (\s@Stack' {} a -> s {storageConnectors = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | The description to display.
stack_description :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_description = Lens.lens (\Stack' {description} -> description) (\s@Stack' {} a -> s {description = a} :: Stack)

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
stack_embedHostDomains :: Lens.Lens' Stack (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
stack_embedHostDomains = Lens.lens (\Stack' {embedHostDomains} -> embedHostDomains) (\s@Stack' {} a -> s {embedHostDomains = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | The errors for the stack.
stack_stackErrors :: Lens.Lens' Stack (Prelude.Maybe [StackError])
stack_stackErrors = Lens.lens (\Stack' {stackErrors} -> stackErrors) (\s@Stack' {} a -> s {stackErrors = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | The stack name to display.
stack_displayName :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_displayName = Lens.lens (\Stack' {displayName} -> displayName) (\s@Stack' {} a -> s {displayName = a} :: Stack)

-- | The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
stack_feedbackURL :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_feedbackURL = Lens.lens (\Stack' {feedbackURL} -> feedbackURL) (\s@Stack' {} a -> s {feedbackURL = a} :: Stack)

-- | The name of the stack.
stack_name :: Lens.Lens' Stack Prelude.Text
stack_name = Lens.lens (\Stack' {name} -> name) (\s@Stack' {} a -> s {name = a} :: Stack)

instance Core.FromJSON Stack where
  parseJSON =
    Core.withObject
      "Stack"
      ( \x ->
          Stack'
            Prelude.<$> (x Core..:? "UserSettings")
            Prelude.<*> (x Core..:? "AccessEndpoints")
            Prelude.<*> (x Core..:? "RedirectURL")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "ApplicationSettings")
            Prelude.<*> ( x Core..:? "StorageConnectors"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "EmbedHostDomains")
            Prelude.<*> (x Core..:? "StackErrors" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "FeedbackURL")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable Stack

instance Prelude.NFData Stack
