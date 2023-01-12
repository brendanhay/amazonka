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
-- Module      : Amazonka.AppStream.UpdateStack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified fields for the specified stack.
module Amazonka.AppStream.UpdateStack
  ( -- * Creating a Request
    UpdateStack (..),
    newUpdateStack,

    -- * Request Lenses
    updateStack_accessEndpoints,
    updateStack_applicationSettings,
    updateStack_attributesToDelete,
    updateStack_deleteStorageConnectors,
    updateStack_description,
    updateStack_displayName,
    updateStack_embedHostDomains,
    updateStack_feedbackURL,
    updateStack_redirectURL,
    updateStack_storageConnectors,
    updateStack_streamingExperienceSettings,
    updateStack_userSettings,
    updateStack_name,

    -- * Destructuring the Response
    UpdateStackResponse (..),
    newUpdateStackResponse,

    -- * Response Lenses
    updateStackResponse_stack,
    updateStackResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { -- | The list of interface VPC endpoint (interface endpoint) objects. Users
    -- of the stack can connect to AppStream 2.0 only through the specified
    -- endpoints.
    accessEndpoints :: Prelude.Maybe (Prelude.NonEmpty AccessEndpoint),
    -- | The persistent application settings for users of a stack. When these
    -- settings are enabled, changes that users make to applications and
    -- Windows settings are automatically saved after each session and applied
    -- to the next session.
    applicationSettings :: Prelude.Maybe ApplicationSettings,
    -- | The stack attributes to delete.
    attributesToDelete :: Prelude.Maybe [StackAttribute],
    -- | Deletes the storage connectors currently enabled for the stack.
    deleteStorageConnectors :: Prelude.Maybe Prelude.Bool,
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The stack name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an
    -- iframe. You must approve the domains that you want to host embedded
    -- AppStream 2.0 streaming sessions.
    embedHostDomains :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The URL that users are redirected to after they choose the Send Feedback
    -- link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Prelude.Maybe Prelude.Text,
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Prelude.Maybe Prelude.Text,
    -- | The storage connectors to enable.
    storageConnectors :: Prelude.Maybe [StorageConnector],
    -- | The streaming protocol you want your stack to prefer. This can be UDP or
    -- TCP. Currently, UDP is only supported in the Windows native client.
    streamingExperienceSettings :: Prelude.Maybe StreamingExperienceSettings,
    -- | The actions that are enabled or disabled for users during their
    -- streaming sessions. By default, these actions are enabled.
    userSettings :: Prelude.Maybe (Prelude.NonEmpty UserSetting),
    -- | The name of the stack.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'applicationSettings', 'updateStack_applicationSettings' - The persistent application settings for users of a stack. When these
-- settings are enabled, changes that users make to applications and
-- Windows settings are automatically saved after each session and applied
-- to the next session.
--
-- 'attributesToDelete', 'updateStack_attributesToDelete' - The stack attributes to delete.
--
-- 'deleteStorageConnectors', 'updateStack_deleteStorageConnectors' - Deletes the storage connectors currently enabled for the stack.
--
-- 'description', 'updateStack_description' - The description to display.
--
-- 'displayName', 'updateStack_displayName' - The stack name to display.
--
-- 'embedHostDomains', 'updateStack_embedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
--
-- 'feedbackURL', 'updateStack_feedbackURL' - The URL that users are redirected to after they choose the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
--
-- 'redirectURL', 'updateStack_redirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- 'storageConnectors', 'updateStack_storageConnectors' - The storage connectors to enable.
--
-- 'streamingExperienceSettings', 'updateStack_streamingExperienceSettings' - The streaming protocol you want your stack to prefer. This can be UDP or
-- TCP. Currently, UDP is only supported in the Windows native client.
--
-- 'userSettings', 'updateStack_userSettings' - The actions that are enabled or disabled for users during their
-- streaming sessions. By default, these actions are enabled.
--
-- 'name', 'updateStack_name' - The name of the stack.
newUpdateStack ::
  -- | 'name'
  Prelude.Text ->
  UpdateStack
newUpdateStack pName_ =
  UpdateStack'
    { accessEndpoints = Prelude.Nothing,
      applicationSettings = Prelude.Nothing,
      attributesToDelete = Prelude.Nothing,
      deleteStorageConnectors = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      embedHostDomains = Prelude.Nothing,
      feedbackURL = Prelude.Nothing,
      redirectURL = Prelude.Nothing,
      storageConnectors = Prelude.Nothing,
      streamingExperienceSettings = Prelude.Nothing,
      userSettings = Prelude.Nothing,
      name = pName_
    }

-- | The list of interface VPC endpoint (interface endpoint) objects. Users
-- of the stack can connect to AppStream 2.0 only through the specified
-- endpoints.
updateStack_accessEndpoints :: Lens.Lens' UpdateStack (Prelude.Maybe (Prelude.NonEmpty AccessEndpoint))
updateStack_accessEndpoints = Lens.lens (\UpdateStack' {accessEndpoints} -> accessEndpoints) (\s@UpdateStack' {} a -> s {accessEndpoints = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | The persistent application settings for users of a stack. When these
-- settings are enabled, changes that users make to applications and
-- Windows settings are automatically saved after each session and applied
-- to the next session.
updateStack_applicationSettings :: Lens.Lens' UpdateStack (Prelude.Maybe ApplicationSettings)
updateStack_applicationSettings = Lens.lens (\UpdateStack' {applicationSettings} -> applicationSettings) (\s@UpdateStack' {} a -> s {applicationSettings = a} :: UpdateStack)

-- | The stack attributes to delete.
updateStack_attributesToDelete :: Lens.Lens' UpdateStack (Prelude.Maybe [StackAttribute])
updateStack_attributesToDelete = Lens.lens (\UpdateStack' {attributesToDelete} -> attributesToDelete) (\s@UpdateStack' {} a -> s {attributesToDelete = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | Deletes the storage connectors currently enabled for the stack.
updateStack_deleteStorageConnectors :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Bool)
updateStack_deleteStorageConnectors = Lens.lens (\UpdateStack' {deleteStorageConnectors} -> deleteStorageConnectors) (\s@UpdateStack' {} a -> s {deleteStorageConnectors = a} :: UpdateStack)

-- | The description to display.
updateStack_description :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_description = Lens.lens (\UpdateStack' {description} -> description) (\s@UpdateStack' {} a -> s {description = a} :: UpdateStack)

-- | The stack name to display.
updateStack_displayName :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_displayName = Lens.lens (\UpdateStack' {displayName} -> displayName) (\s@UpdateStack' {} a -> s {displayName = a} :: UpdateStack)

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
updateStack_embedHostDomains :: Lens.Lens' UpdateStack (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateStack_embedHostDomains = Lens.lens (\UpdateStack' {embedHostDomains} -> embedHostDomains) (\s@UpdateStack' {} a -> s {embedHostDomains = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | The URL that users are redirected to after they choose the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
updateStack_feedbackURL :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_feedbackURL = Lens.lens (\UpdateStack' {feedbackURL} -> feedbackURL) (\s@UpdateStack' {} a -> s {feedbackURL = a} :: UpdateStack)

-- | The URL that users are redirected to after their streaming session ends.
updateStack_redirectURL :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_redirectURL = Lens.lens (\UpdateStack' {redirectURL} -> redirectURL) (\s@UpdateStack' {} a -> s {redirectURL = a} :: UpdateStack)

-- | The storage connectors to enable.
updateStack_storageConnectors :: Lens.Lens' UpdateStack (Prelude.Maybe [StorageConnector])
updateStack_storageConnectors = Lens.lens (\UpdateStack' {storageConnectors} -> storageConnectors) (\s@UpdateStack' {} a -> s {storageConnectors = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | The streaming protocol you want your stack to prefer. This can be UDP or
-- TCP. Currently, UDP is only supported in the Windows native client.
updateStack_streamingExperienceSettings :: Lens.Lens' UpdateStack (Prelude.Maybe StreamingExperienceSettings)
updateStack_streamingExperienceSettings = Lens.lens (\UpdateStack' {streamingExperienceSettings} -> streamingExperienceSettings) (\s@UpdateStack' {} a -> s {streamingExperienceSettings = a} :: UpdateStack)

-- | The actions that are enabled or disabled for users during their
-- streaming sessions. By default, these actions are enabled.
updateStack_userSettings :: Lens.Lens' UpdateStack (Prelude.Maybe (Prelude.NonEmpty UserSetting))
updateStack_userSettings = Lens.lens (\UpdateStack' {userSettings} -> userSettings) (\s@UpdateStack' {} a -> s {userSettings = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stack.
updateStack_name :: Lens.Lens' UpdateStack Prelude.Text
updateStack_name = Lens.lens (\UpdateStack' {name} -> name) (\s@UpdateStack' {} a -> s {name = a} :: UpdateStack)

instance Core.AWSRequest UpdateStack where
  type AWSResponse UpdateStack = UpdateStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStackResponse'
            Prelude.<$> (x Data..?> "Stack")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStack where
  hashWithSalt _salt UpdateStack' {..} =
    _salt `Prelude.hashWithSalt` accessEndpoints
      `Prelude.hashWithSalt` applicationSettings
      `Prelude.hashWithSalt` attributesToDelete
      `Prelude.hashWithSalt` deleteStorageConnectors
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` embedHostDomains
      `Prelude.hashWithSalt` feedbackURL
      `Prelude.hashWithSalt` redirectURL
      `Prelude.hashWithSalt` storageConnectors
      `Prelude.hashWithSalt` streamingExperienceSettings
      `Prelude.hashWithSalt` userSettings
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateStack where
  rnf UpdateStack' {..} =
    Prelude.rnf accessEndpoints
      `Prelude.seq` Prelude.rnf applicationSettings
      `Prelude.seq` Prelude.rnf attributesToDelete
      `Prelude.seq` Prelude.rnf deleteStorageConnectors
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf embedHostDomains
      `Prelude.seq` Prelude.rnf feedbackURL
      `Prelude.seq` Prelude.rnf redirectURL
      `Prelude.seq` Prelude.rnf storageConnectors
      `Prelude.seq` Prelude.rnf streamingExperienceSettings
      `Prelude.seq` Prelude.rnf userSettings
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.UpdateStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStack where
  toJSON UpdateStack' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessEndpoints" Data..=)
              Prelude.<$> accessEndpoints,
            ("ApplicationSettings" Data..=)
              Prelude.<$> applicationSettings,
            ("AttributesToDelete" Data..=)
              Prelude.<$> attributesToDelete,
            ("DeleteStorageConnectors" Data..=)
              Prelude.<$> deleteStorageConnectors,
            ("Description" Data..=) Prelude.<$> description,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("EmbedHostDomains" Data..=)
              Prelude.<$> embedHostDomains,
            ("FeedbackURL" Data..=) Prelude.<$> feedbackURL,
            ("RedirectURL" Data..=) Prelude.<$> redirectURL,
            ("StorageConnectors" Data..=)
              Prelude.<$> storageConnectors,
            ("StreamingExperienceSettings" Data..=)
              Prelude.<$> streamingExperienceSettings,
            ("UserSettings" Data..=) Prelude.<$> userSettings,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateStack where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { -- | Information about the stack.
    stack :: Prelude.Maybe Stack,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateStackResponse
newUpdateStackResponse pHttpStatus_ =
  UpdateStackResponse'
    { stack = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the stack.
updateStackResponse_stack :: Lens.Lens' UpdateStackResponse (Prelude.Maybe Stack)
updateStackResponse_stack = Lens.lens (\UpdateStackResponse' {stack} -> stack) (\s@UpdateStackResponse' {} a -> s {stack = a} :: UpdateStackResponse)

-- | The response's http status code.
updateStackResponse_httpStatus :: Lens.Lens' UpdateStackResponse Prelude.Int
updateStackResponse_httpStatus = Lens.lens (\UpdateStackResponse' {httpStatus} -> httpStatus) (\s@UpdateStackResponse' {} a -> s {httpStatus = a} :: UpdateStackResponse)

instance Prelude.NFData UpdateStackResponse where
  rnf UpdateStackResponse' {..} =
    Prelude.rnf stack
      `Prelude.seq` Prelude.rnf httpStatus
