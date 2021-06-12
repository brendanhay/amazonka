{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.UpdateStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified fields for the specified stack.
module Network.AWS.AppStream.UpdateStack
  ( -- * Creating a Request
    UpdateStack (..),
    newUpdateStack,

    -- * Request Lenses
    updateStack_accessEndpoints,
    updateStack_userSettings,
    updateStack_redirectURL,
    updateStack_applicationSettings,
    updateStack_storageConnectors,
    updateStack_description,
    updateStack_embedHostDomains,
    updateStack_deleteStorageConnectors,
    updateStack_displayName,
    updateStack_attributesToDelete,
    updateStack_feedbackURL,
    updateStack_name,

    -- * Destructuring the Response
    UpdateStackResponse (..),
    newUpdateStackResponse,

    -- * Response Lenses
    updateStackResponse_stack,
    updateStackResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { -- | The list of interface VPC endpoint (interface endpoint) objects. Users
    -- of the stack can connect to AppStream 2.0 only through the specified
    -- endpoints.
    accessEndpoints :: Core.Maybe (Core.NonEmpty AccessEndpoint),
    -- | The actions that are enabled or disabled for users during their
    -- streaming sessions. By default, these actions are enabled.
    userSettings :: Core.Maybe (Core.NonEmpty UserSetting),
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Core.Maybe Core.Text,
    -- | The persistent application settings for users of a stack. When these
    -- settings are enabled, changes that users make to applications and
    -- Windows settings are automatically saved after each session and applied
    -- to the next session.
    applicationSettings :: Core.Maybe ApplicationSettings,
    -- | The storage connectors to enable.
    storageConnectors :: Core.Maybe [StorageConnector],
    -- | The description to display.
    description :: Core.Maybe Core.Text,
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an
    -- iframe. You must approve the domains that you want to host embedded
    -- AppStream 2.0 streaming sessions.
    embedHostDomains :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Deletes the storage connectors currently enabled for the stack.
    deleteStorageConnectors :: Core.Maybe Core.Bool,
    -- | The stack name to display.
    displayName :: Core.Maybe Core.Text,
    -- | The stack attributes to delete.
    attributesToDelete :: Core.Maybe [StackAttribute],
    -- | The URL that users are redirected to after they choose the Send Feedback
    -- link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Core.Maybe Core.Text,
    -- | The name of the stack.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessEndpoints', 'updateStack_accessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects. Users
-- of the stack can connect to AppStream 2.0 only through the specified
-- endpoints.
--
-- 'userSettings', 'updateStack_userSettings' - The actions that are enabled or disabled for users during their
-- streaming sessions. By default, these actions are enabled.
--
-- 'redirectURL', 'updateStack_redirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- 'applicationSettings', 'updateStack_applicationSettings' - The persistent application settings for users of a stack. When these
-- settings are enabled, changes that users make to applications and
-- Windows settings are automatically saved after each session and applied
-- to the next session.
--
-- 'storageConnectors', 'updateStack_storageConnectors' - The storage connectors to enable.
--
-- 'description', 'updateStack_description' - The description to display.
--
-- 'embedHostDomains', 'updateStack_embedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
--
-- 'deleteStorageConnectors', 'updateStack_deleteStorageConnectors' - Deletes the storage connectors currently enabled for the stack.
--
-- 'displayName', 'updateStack_displayName' - The stack name to display.
--
-- 'attributesToDelete', 'updateStack_attributesToDelete' - The stack attributes to delete.
--
-- 'feedbackURL', 'updateStack_feedbackURL' - The URL that users are redirected to after they choose the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
--
-- 'name', 'updateStack_name' - The name of the stack.
newUpdateStack ::
  -- | 'name'
  Core.Text ->
  UpdateStack
newUpdateStack pName_ =
  UpdateStack'
    { accessEndpoints = Core.Nothing,
      userSettings = Core.Nothing,
      redirectURL = Core.Nothing,
      applicationSettings = Core.Nothing,
      storageConnectors = Core.Nothing,
      description = Core.Nothing,
      embedHostDomains = Core.Nothing,
      deleteStorageConnectors = Core.Nothing,
      displayName = Core.Nothing,
      attributesToDelete = Core.Nothing,
      feedbackURL = Core.Nothing,
      name = pName_
    }

-- | The list of interface VPC endpoint (interface endpoint) objects. Users
-- of the stack can connect to AppStream 2.0 only through the specified
-- endpoints.
updateStack_accessEndpoints :: Lens.Lens' UpdateStack (Core.Maybe (Core.NonEmpty AccessEndpoint))
updateStack_accessEndpoints = Lens.lens (\UpdateStack' {accessEndpoints} -> accessEndpoints) (\s@UpdateStack' {} a -> s {accessEndpoints = a} :: UpdateStack) Core.. Lens.mapping Lens._Coerce

-- | The actions that are enabled or disabled for users during their
-- streaming sessions. By default, these actions are enabled.
updateStack_userSettings :: Lens.Lens' UpdateStack (Core.Maybe (Core.NonEmpty UserSetting))
updateStack_userSettings = Lens.lens (\UpdateStack' {userSettings} -> userSettings) (\s@UpdateStack' {} a -> s {userSettings = a} :: UpdateStack) Core.. Lens.mapping Lens._Coerce

-- | The URL that users are redirected to after their streaming session ends.
updateStack_redirectURL :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_redirectURL = Lens.lens (\UpdateStack' {redirectURL} -> redirectURL) (\s@UpdateStack' {} a -> s {redirectURL = a} :: UpdateStack)

-- | The persistent application settings for users of a stack. When these
-- settings are enabled, changes that users make to applications and
-- Windows settings are automatically saved after each session and applied
-- to the next session.
updateStack_applicationSettings :: Lens.Lens' UpdateStack (Core.Maybe ApplicationSettings)
updateStack_applicationSettings = Lens.lens (\UpdateStack' {applicationSettings} -> applicationSettings) (\s@UpdateStack' {} a -> s {applicationSettings = a} :: UpdateStack)

-- | The storage connectors to enable.
updateStack_storageConnectors :: Lens.Lens' UpdateStack (Core.Maybe [StorageConnector])
updateStack_storageConnectors = Lens.lens (\UpdateStack' {storageConnectors} -> storageConnectors) (\s@UpdateStack' {} a -> s {storageConnectors = a} :: UpdateStack) Core.. Lens.mapping Lens._Coerce

-- | The description to display.
updateStack_description :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_description = Lens.lens (\UpdateStack' {description} -> description) (\s@UpdateStack' {} a -> s {description = a} :: UpdateStack)

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
updateStack_embedHostDomains :: Lens.Lens' UpdateStack (Core.Maybe (Core.NonEmpty Core.Text))
updateStack_embedHostDomains = Lens.lens (\UpdateStack' {embedHostDomains} -> embedHostDomains) (\s@UpdateStack' {} a -> s {embedHostDomains = a} :: UpdateStack) Core.. Lens.mapping Lens._Coerce

-- | Deletes the storage connectors currently enabled for the stack.
updateStack_deleteStorageConnectors :: Lens.Lens' UpdateStack (Core.Maybe Core.Bool)
updateStack_deleteStorageConnectors = Lens.lens (\UpdateStack' {deleteStorageConnectors} -> deleteStorageConnectors) (\s@UpdateStack' {} a -> s {deleteStorageConnectors = a} :: UpdateStack)

-- | The stack name to display.
updateStack_displayName :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_displayName = Lens.lens (\UpdateStack' {displayName} -> displayName) (\s@UpdateStack' {} a -> s {displayName = a} :: UpdateStack)

-- | The stack attributes to delete.
updateStack_attributesToDelete :: Lens.Lens' UpdateStack (Core.Maybe [StackAttribute])
updateStack_attributesToDelete = Lens.lens (\UpdateStack' {attributesToDelete} -> attributesToDelete) (\s@UpdateStack' {} a -> s {attributesToDelete = a} :: UpdateStack) Core.. Lens.mapping Lens._Coerce

-- | The URL that users are redirected to after they choose the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
updateStack_feedbackURL :: Lens.Lens' UpdateStack (Core.Maybe Core.Text)
updateStack_feedbackURL = Lens.lens (\UpdateStack' {feedbackURL} -> feedbackURL) (\s@UpdateStack' {} a -> s {feedbackURL = a} :: UpdateStack)

-- | The name of the stack.
updateStack_name :: Lens.Lens' UpdateStack Core.Text
updateStack_name = Lens.lens (\UpdateStack' {name} -> name) (\s@UpdateStack' {} a -> s {name = a} :: UpdateStack)

instance Core.AWSRequest UpdateStack where
  type AWSResponse UpdateStack = UpdateStackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStackResponse'
            Core.<$> (x Core..?> "Stack")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateStack

instance Core.NFData UpdateStack

instance Core.ToHeaders UpdateStack where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.UpdateStack" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateStack where
  toJSON UpdateStack' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessEndpoints" Core..=)
              Core.<$> accessEndpoints,
            ("UserSettings" Core..=) Core.<$> userSettings,
            ("RedirectURL" Core..=) Core.<$> redirectURL,
            ("ApplicationSettings" Core..=)
              Core.<$> applicationSettings,
            ("StorageConnectors" Core..=)
              Core.<$> storageConnectors,
            ("Description" Core..=) Core.<$> description,
            ("EmbedHostDomains" Core..=)
              Core.<$> embedHostDomains,
            ("DeleteStorageConnectors" Core..=)
              Core.<$> deleteStorageConnectors,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("AttributesToDelete" Core..=)
              Core.<$> attributesToDelete,
            ("FeedbackURL" Core..=) Core.<$> feedbackURL,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateStack where
  toPath = Core.const "/"

instance Core.ToQuery UpdateStack where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { -- | Information about the stack.
    stack :: Core.Maybe Stack,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stack', 'updateStackResponse_stack' - Information about the stack.
--
-- 'httpStatus', 'updateStackResponse_httpStatus' - The response's http status code.
newUpdateStackResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateStackResponse
newUpdateStackResponse pHttpStatus_ =
  UpdateStackResponse'
    { stack = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the stack.
updateStackResponse_stack :: Lens.Lens' UpdateStackResponse (Core.Maybe Stack)
updateStackResponse_stack = Lens.lens (\UpdateStackResponse' {stack} -> stack) (\s@UpdateStackResponse' {} a -> s {stack = a} :: UpdateStackResponse)

-- | The response's http status code.
updateStackResponse_httpStatus :: Lens.Lens' UpdateStackResponse Core.Int
updateStackResponse_httpStatus = Lens.lens (\UpdateStackResponse' {httpStatus} -> httpStatus) (\s@UpdateStackResponse' {} a -> s {httpStatus = a} :: UpdateStackResponse)

instance Core.NFData UpdateStackResponse
