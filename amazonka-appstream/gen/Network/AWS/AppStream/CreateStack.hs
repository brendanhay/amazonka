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
-- Module      : Network.AWS.AppStream.CreateStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack to start streaming applications to users. A stack
-- consists of an associated fleet, user access policies, and storage
-- configurations.
module Network.AWS.AppStream.CreateStack
  ( -- * Creating a Request
    CreateStack (..),
    newCreateStack,

    -- * Request Lenses
    createStack_accessEndpoints,
    createStack_userSettings,
    createStack_redirectURL,
    createStack_applicationSettings,
    createStack_tags,
    createStack_storageConnectors,
    createStack_description,
    createStack_embedHostDomains,
    createStack_displayName,
    createStack_feedbackURL,
    createStack_name,

    -- * Destructuring the Response
    CreateStackResponse (..),
    newCreateStackResponse,

    -- * Response Lenses
    createStackResponse_stack,
    createStackResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStack' smart constructor.
data CreateStack = CreateStack'
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
    -- | The tags to associate with the stack. A tag is a key-value pair, and the
    -- value is optional. For example, Environment=Test. If you do not specify
    -- a value, Environment=.
    --
    -- If you do not specify a value, the value is set to an empty string.
    --
    -- Generally allowed characters are: letters, numbers, and spaces
    -- representable in UTF-8, and the following special characters:
    --
    -- _ . : \/ = + \\ - \@
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
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
    -- | The URL that users are redirected to after they click the Send Feedback
    -- link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Core.Maybe Core.Text,
    -- | The name of the stack.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessEndpoints', 'createStack_accessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects. Users
-- of the stack can connect to AppStream 2.0 only through the specified
-- endpoints.
--
-- 'userSettings', 'createStack_userSettings' - The actions that are enabled or disabled for users during their
-- streaming sessions. By default, these actions are enabled.
--
-- 'redirectURL', 'createStack_redirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- 'applicationSettings', 'createStack_applicationSettings' - The persistent application settings for users of a stack. When these
-- settings are enabled, changes that users make to applications and
-- Windows settings are automatically saved after each session and applied
-- to the next session.
--
-- 'tags', 'createStack_tags' - The tags to associate with the stack. A tag is a key-value pair, and the
-- value is optional. For example, Environment=Test. If you do not specify
-- a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
--
-- Generally allowed characters are: letters, numbers, and spaces
-- representable in UTF-8, and the following special characters:
--
-- _ . : \/ = + \\ - \@
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- 'storageConnectors', 'createStack_storageConnectors' - The storage connectors to enable.
--
-- 'description', 'createStack_description' - The description to display.
--
-- 'embedHostDomains', 'createStack_embedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
--
-- 'displayName', 'createStack_displayName' - The stack name to display.
--
-- 'feedbackURL', 'createStack_feedbackURL' - The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
--
-- 'name', 'createStack_name' - The name of the stack.
newCreateStack ::
  -- | 'name'
  Core.Text ->
  CreateStack
newCreateStack pName_ =
  CreateStack'
    { accessEndpoints = Core.Nothing,
      userSettings = Core.Nothing,
      redirectURL = Core.Nothing,
      applicationSettings = Core.Nothing,
      tags = Core.Nothing,
      storageConnectors = Core.Nothing,
      description = Core.Nothing,
      embedHostDomains = Core.Nothing,
      displayName = Core.Nothing,
      feedbackURL = Core.Nothing,
      name = pName_
    }

-- | The list of interface VPC endpoint (interface endpoint) objects. Users
-- of the stack can connect to AppStream 2.0 only through the specified
-- endpoints.
createStack_accessEndpoints :: Lens.Lens' CreateStack (Core.Maybe (Core.NonEmpty AccessEndpoint))
createStack_accessEndpoints = Lens.lens (\CreateStack' {accessEndpoints} -> accessEndpoints) (\s@CreateStack' {} a -> s {accessEndpoints = a} :: CreateStack) Core.. Lens.mapping Lens._Coerce

-- | The actions that are enabled or disabled for users during their
-- streaming sessions. By default, these actions are enabled.
createStack_userSettings :: Lens.Lens' CreateStack (Core.Maybe (Core.NonEmpty UserSetting))
createStack_userSettings = Lens.lens (\CreateStack' {userSettings} -> userSettings) (\s@CreateStack' {} a -> s {userSettings = a} :: CreateStack) Core.. Lens.mapping Lens._Coerce

-- | The URL that users are redirected to after their streaming session ends.
createStack_redirectURL :: Lens.Lens' CreateStack (Core.Maybe Core.Text)
createStack_redirectURL = Lens.lens (\CreateStack' {redirectURL} -> redirectURL) (\s@CreateStack' {} a -> s {redirectURL = a} :: CreateStack)

-- | The persistent application settings for users of a stack. When these
-- settings are enabled, changes that users make to applications and
-- Windows settings are automatically saved after each session and applied
-- to the next session.
createStack_applicationSettings :: Lens.Lens' CreateStack (Core.Maybe ApplicationSettings)
createStack_applicationSettings = Lens.lens (\CreateStack' {applicationSettings} -> applicationSettings) (\s@CreateStack' {} a -> s {applicationSettings = a} :: CreateStack)

-- | The tags to associate with the stack. A tag is a key-value pair, and the
-- value is optional. For example, Environment=Test. If you do not specify
-- a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
--
-- Generally allowed characters are: letters, numbers, and spaces
-- representable in UTF-8, and the following special characters:
--
-- _ . : \/ = + \\ - \@
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
-- in the /Amazon AppStream 2.0 Administration Guide/.
createStack_tags :: Lens.Lens' CreateStack (Core.Maybe (Core.HashMap Core.Text Core.Text))
createStack_tags = Lens.lens (\CreateStack' {tags} -> tags) (\s@CreateStack' {} a -> s {tags = a} :: CreateStack) Core.. Lens.mapping Lens._Coerce

-- | The storage connectors to enable.
createStack_storageConnectors :: Lens.Lens' CreateStack (Core.Maybe [StorageConnector])
createStack_storageConnectors = Lens.lens (\CreateStack' {storageConnectors} -> storageConnectors) (\s@CreateStack' {} a -> s {storageConnectors = a} :: CreateStack) Core.. Lens.mapping Lens._Coerce

-- | The description to display.
createStack_description :: Lens.Lens' CreateStack (Core.Maybe Core.Text)
createStack_description = Lens.lens (\CreateStack' {description} -> description) (\s@CreateStack' {} a -> s {description = a} :: CreateStack)

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
createStack_embedHostDomains :: Lens.Lens' CreateStack (Core.Maybe (Core.NonEmpty Core.Text))
createStack_embedHostDomains = Lens.lens (\CreateStack' {embedHostDomains} -> embedHostDomains) (\s@CreateStack' {} a -> s {embedHostDomains = a} :: CreateStack) Core.. Lens.mapping Lens._Coerce

-- | The stack name to display.
createStack_displayName :: Lens.Lens' CreateStack (Core.Maybe Core.Text)
createStack_displayName = Lens.lens (\CreateStack' {displayName} -> displayName) (\s@CreateStack' {} a -> s {displayName = a} :: CreateStack)

-- | The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
createStack_feedbackURL :: Lens.Lens' CreateStack (Core.Maybe Core.Text)
createStack_feedbackURL = Lens.lens (\CreateStack' {feedbackURL} -> feedbackURL) (\s@CreateStack' {} a -> s {feedbackURL = a} :: CreateStack)

-- | The name of the stack.
createStack_name :: Lens.Lens' CreateStack Core.Text
createStack_name = Lens.lens (\CreateStack' {name} -> name) (\s@CreateStack' {} a -> s {name = a} :: CreateStack)

instance Core.AWSRequest CreateStack where
  type AWSResponse CreateStack = CreateStackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStackResponse'
            Core.<$> (x Core..?> "Stack")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateStack

instance Core.NFData CreateStack

instance Core.ToHeaders CreateStack where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateStack" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateStack where
  toJSON CreateStack' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessEndpoints" Core..=)
              Core.<$> accessEndpoints,
            ("UserSettings" Core..=) Core.<$> userSettings,
            ("RedirectURL" Core..=) Core.<$> redirectURL,
            ("ApplicationSettings" Core..=)
              Core.<$> applicationSettings,
            ("Tags" Core..=) Core.<$> tags,
            ("StorageConnectors" Core..=)
              Core.<$> storageConnectors,
            ("Description" Core..=) Core.<$> description,
            ("EmbedHostDomains" Core..=)
              Core.<$> embedHostDomains,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("FeedbackURL" Core..=) Core.<$> feedbackURL,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateStack where
  toPath = Core.const "/"

instance Core.ToQuery CreateStack where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { -- | Information about the stack.
    stack :: Core.Maybe Stack,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stack', 'createStackResponse_stack' - Information about the stack.
--
-- 'httpStatus', 'createStackResponse_httpStatus' - The response's http status code.
newCreateStackResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateStackResponse
newCreateStackResponse pHttpStatus_ =
  CreateStackResponse'
    { stack = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the stack.
createStackResponse_stack :: Lens.Lens' CreateStackResponse (Core.Maybe Stack)
createStackResponse_stack = Lens.lens (\CreateStackResponse' {stack} -> stack) (\s@CreateStackResponse' {} a -> s {stack = a} :: CreateStackResponse)

-- | The response's http status code.
createStackResponse_httpStatus :: Lens.Lens' CreateStackResponse Core.Int
createStackResponse_httpStatus = Lens.lens (\CreateStackResponse' {httpStatus} -> httpStatus) (\s@CreateStackResponse' {} a -> s {httpStatus = a} :: CreateStackResponse)

instance Core.NFData CreateStackResponse
