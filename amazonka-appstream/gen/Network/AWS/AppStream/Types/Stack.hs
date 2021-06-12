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

-- | Describes a stack.
--
-- /See:/ 'newStack' smart constructor.
data Stack = Stack'
  { -- | The list of virtual private cloud (VPC) interface endpoint objects.
    -- Users of the stack can connect to AppStream 2.0 only through the
    -- specified endpoints.
    accessEndpoints :: Core.Maybe (Core.NonEmpty AccessEndpoint),
    -- | The actions that are enabled or disabled for users during their
    -- streaming sessions. By default these actions are enabled.
    userSettings :: Core.Maybe (Core.NonEmpty UserSetting),
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Core.Maybe Core.Text,
    -- | The ARN of the stack.
    arn :: Core.Maybe Core.Text,
    -- | The time the stack was created.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The persistent application settings for users of the stack.
    applicationSettings :: Core.Maybe ApplicationSettingsResponse,
    -- | The storage connectors to enable.
    storageConnectors :: Core.Maybe [StorageConnector],
    -- | The description to display.
    description :: Core.Maybe Core.Text,
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an
    -- iframe. You must approve the domains that you want to host embedded
    -- AppStream 2.0 streaming sessions.
    embedHostDomains :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The stack name to display.
    displayName :: Core.Maybe Core.Text,
    -- | The errors for the stack.
    stackErrors :: Core.Maybe [StackError],
    -- | The URL that users are redirected to after they click the Send Feedback
    -- link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Core.Maybe Core.Text,
    -- | The name of the stack.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'userSettings', 'stack_userSettings' - The actions that are enabled or disabled for users during their
-- streaming sessions. By default these actions are enabled.
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
-- 'displayName', 'stack_displayName' - The stack name to display.
--
-- 'stackErrors', 'stack_stackErrors' - The errors for the stack.
--
-- 'feedbackURL', 'stack_feedbackURL' - The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
--
-- 'name', 'stack_name' - The name of the stack.
newStack ::
  -- | 'name'
  Core.Text ->
  Stack
newStack pName_ =
  Stack'
    { accessEndpoints = Core.Nothing,
      userSettings = Core.Nothing,
      redirectURL = Core.Nothing,
      arn = Core.Nothing,
      createdTime = Core.Nothing,
      applicationSettings = Core.Nothing,
      storageConnectors = Core.Nothing,
      description = Core.Nothing,
      embedHostDomains = Core.Nothing,
      displayName = Core.Nothing,
      stackErrors = Core.Nothing,
      feedbackURL = Core.Nothing,
      name = pName_
    }

-- | The list of virtual private cloud (VPC) interface endpoint objects.
-- Users of the stack can connect to AppStream 2.0 only through the
-- specified endpoints.
stack_accessEndpoints :: Lens.Lens' Stack (Core.Maybe (Core.NonEmpty AccessEndpoint))
stack_accessEndpoints = Lens.lens (\Stack' {accessEndpoints} -> accessEndpoints) (\s@Stack' {} a -> s {accessEndpoints = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | The actions that are enabled or disabled for users during their
-- streaming sessions. By default these actions are enabled.
stack_userSettings :: Lens.Lens' Stack (Core.Maybe (Core.NonEmpty UserSetting))
stack_userSettings = Lens.lens (\Stack' {userSettings} -> userSettings) (\s@Stack' {} a -> s {userSettings = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | The URL that users are redirected to after their streaming session ends.
stack_redirectURL :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_redirectURL = Lens.lens (\Stack' {redirectURL} -> redirectURL) (\s@Stack' {} a -> s {redirectURL = a} :: Stack)

-- | The ARN of the stack.
stack_arn :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_arn = Lens.lens (\Stack' {arn} -> arn) (\s@Stack' {} a -> s {arn = a} :: Stack)

-- | The time the stack was created.
stack_createdTime :: Lens.Lens' Stack (Core.Maybe Core.UTCTime)
stack_createdTime = Lens.lens (\Stack' {createdTime} -> createdTime) (\s@Stack' {} a -> s {createdTime = a} :: Stack) Core.. Lens.mapping Core._Time

-- | The persistent application settings for users of the stack.
stack_applicationSettings :: Lens.Lens' Stack (Core.Maybe ApplicationSettingsResponse)
stack_applicationSettings = Lens.lens (\Stack' {applicationSettings} -> applicationSettings) (\s@Stack' {} a -> s {applicationSettings = a} :: Stack)

-- | The storage connectors to enable.
stack_storageConnectors :: Lens.Lens' Stack (Core.Maybe [StorageConnector])
stack_storageConnectors = Lens.lens (\Stack' {storageConnectors} -> storageConnectors) (\s@Stack' {} a -> s {storageConnectors = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | The description to display.
stack_description :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_description = Lens.lens (\Stack' {description} -> description) (\s@Stack' {} a -> s {description = a} :: Stack)

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
stack_embedHostDomains :: Lens.Lens' Stack (Core.Maybe (Core.NonEmpty Core.Text))
stack_embedHostDomains = Lens.lens (\Stack' {embedHostDomains} -> embedHostDomains) (\s@Stack' {} a -> s {embedHostDomains = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | The stack name to display.
stack_displayName :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_displayName = Lens.lens (\Stack' {displayName} -> displayName) (\s@Stack' {} a -> s {displayName = a} :: Stack)

-- | The errors for the stack.
stack_stackErrors :: Lens.Lens' Stack (Core.Maybe [StackError])
stack_stackErrors = Lens.lens (\Stack' {stackErrors} -> stackErrors) (\s@Stack' {} a -> s {stackErrors = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
stack_feedbackURL :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_feedbackURL = Lens.lens (\Stack' {feedbackURL} -> feedbackURL) (\s@Stack' {} a -> s {feedbackURL = a} :: Stack)

-- | The name of the stack.
stack_name :: Lens.Lens' Stack Core.Text
stack_name = Lens.lens (\Stack' {name} -> name) (\s@Stack' {} a -> s {name = a} :: Stack)

instance Core.FromJSON Stack where
  parseJSON =
    Core.withObject
      "Stack"
      ( \x ->
          Stack'
            Core.<$> (x Core..:? "AccessEndpoints")
            Core.<*> (x Core..:? "UserSettings")
            Core.<*> (x Core..:? "RedirectURL")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "ApplicationSettings")
            Core.<*> (x Core..:? "StorageConnectors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "EmbedHostDomains")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "StackErrors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "FeedbackURL")
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable Stack

instance Core.NFData Stack
