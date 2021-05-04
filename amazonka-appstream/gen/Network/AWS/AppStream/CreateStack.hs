{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStack' smart constructor.
data CreateStack = CreateStack'
  { -- | The list of interface VPC endpoint (interface endpoint) objects. Users
    -- of the stack can connect to AppStream 2.0 only through the specified
    -- endpoints.
    accessEndpoints :: Prelude.Maybe (Prelude.NonEmpty AccessEndpoint),
    -- | The actions that are enabled or disabled for users during their
    -- streaming sessions. By default, these actions are enabled.
    userSettings :: Prelude.Maybe (Prelude.NonEmpty UserSetting),
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Prelude.Maybe Prelude.Text,
    -- | The persistent application settings for users of a stack. When these
    -- settings are enabled, changes that users make to applications and
    -- Windows settings are automatically saved after each session and applied
    -- to the next session.
    applicationSettings :: Prelude.Maybe ApplicationSettings,
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
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The storage connectors to enable.
    storageConnectors :: Prelude.Maybe [StorageConnector],
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an
    -- iframe. You must approve the domains that you want to host embedded
    -- AppStream 2.0 streaming sessions.
    embedHostDomains :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The stack name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The URL that users are redirected to after they click the Send Feedback
    -- link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Prelude.Maybe Prelude.Text,
    -- | The name of the stack.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateStack
newCreateStack pName_ =
  CreateStack'
    { accessEndpoints = Prelude.Nothing,
      userSettings = Prelude.Nothing,
      redirectURL = Prelude.Nothing,
      applicationSettings = Prelude.Nothing,
      tags = Prelude.Nothing,
      storageConnectors = Prelude.Nothing,
      description = Prelude.Nothing,
      embedHostDomains = Prelude.Nothing,
      displayName = Prelude.Nothing,
      feedbackURL = Prelude.Nothing,
      name = pName_
    }

-- | The list of interface VPC endpoint (interface endpoint) objects. Users
-- of the stack can connect to AppStream 2.0 only through the specified
-- endpoints.
createStack_accessEndpoints :: Lens.Lens' CreateStack (Prelude.Maybe (Prelude.NonEmpty AccessEndpoint))
createStack_accessEndpoints = Lens.lens (\CreateStack' {accessEndpoints} -> accessEndpoints) (\s@CreateStack' {} a -> s {accessEndpoints = a} :: CreateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | The actions that are enabled or disabled for users during their
-- streaming sessions. By default, these actions are enabled.
createStack_userSettings :: Lens.Lens' CreateStack (Prelude.Maybe (Prelude.NonEmpty UserSetting))
createStack_userSettings = Lens.lens (\CreateStack' {userSettings} -> userSettings) (\s@CreateStack' {} a -> s {userSettings = a} :: CreateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | The URL that users are redirected to after their streaming session ends.
createStack_redirectURL :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_redirectURL = Lens.lens (\CreateStack' {redirectURL} -> redirectURL) (\s@CreateStack' {} a -> s {redirectURL = a} :: CreateStack)

-- | The persistent application settings for users of a stack. When these
-- settings are enabled, changes that users make to applications and
-- Windows settings are automatically saved after each session and applied
-- to the next session.
createStack_applicationSettings :: Lens.Lens' CreateStack (Prelude.Maybe ApplicationSettings)
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
createStack_tags :: Lens.Lens' CreateStack (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStack_tags = Lens.lens (\CreateStack' {tags} -> tags) (\s@CreateStack' {} a -> s {tags = a} :: CreateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | The storage connectors to enable.
createStack_storageConnectors :: Lens.Lens' CreateStack (Prelude.Maybe [StorageConnector])
createStack_storageConnectors = Lens.lens (\CreateStack' {storageConnectors} -> storageConnectors) (\s@CreateStack' {} a -> s {storageConnectors = a} :: CreateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | The description to display.
createStack_description :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_description = Lens.lens (\CreateStack' {description} -> description) (\s@CreateStack' {} a -> s {description = a} :: CreateStack)

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
createStack_embedHostDomains :: Lens.Lens' CreateStack (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createStack_embedHostDomains = Lens.lens (\CreateStack' {embedHostDomains} -> embedHostDomains) (\s@CreateStack' {} a -> s {embedHostDomains = a} :: CreateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | The stack name to display.
createStack_displayName :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_displayName = Lens.lens (\CreateStack' {displayName} -> displayName) (\s@CreateStack' {} a -> s {displayName = a} :: CreateStack)

-- | The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
createStack_feedbackURL :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_feedbackURL = Lens.lens (\CreateStack' {feedbackURL} -> feedbackURL) (\s@CreateStack' {} a -> s {feedbackURL = a} :: CreateStack)

-- | The name of the stack.
createStack_name :: Lens.Lens' CreateStack Prelude.Text
createStack_name = Lens.lens (\CreateStack' {name} -> name) (\s@CreateStack' {} a -> s {name = a} :: CreateStack)

instance Prelude.AWSRequest CreateStack where
  type Rs CreateStack = CreateStackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStackResponse'
            Prelude.<$> (x Prelude..?> "Stack")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStack

instance Prelude.NFData CreateStack

instance Prelude.ToHeaders CreateStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.CreateStack" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateStack where
  toJSON CreateStack' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccessEndpoints" Prelude..=)
              Prelude.<$> accessEndpoints,
            ("UserSettings" Prelude..=) Prelude.<$> userSettings,
            ("RedirectURL" Prelude..=) Prelude.<$> redirectURL,
            ("ApplicationSettings" Prelude..=)
              Prelude.<$> applicationSettings,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("StorageConnectors" Prelude..=)
              Prelude.<$> storageConnectors,
            ("Description" Prelude..=) Prelude.<$> description,
            ("EmbedHostDomains" Prelude..=)
              Prelude.<$> embedHostDomains,
            ("DisplayName" Prelude..=) Prelude.<$> displayName,
            ("FeedbackURL" Prelude..=) Prelude.<$> feedbackURL,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath CreateStack where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { -- | Information about the stack.
    stack :: Prelude.Maybe Stack,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateStackResponse
newCreateStackResponse pHttpStatus_ =
  CreateStackResponse'
    { stack = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the stack.
createStackResponse_stack :: Lens.Lens' CreateStackResponse (Prelude.Maybe Stack)
createStackResponse_stack = Lens.lens (\CreateStackResponse' {stack} -> stack) (\s@CreateStackResponse' {} a -> s {stack = a} :: CreateStackResponse)

-- | The response's http status code.
createStackResponse_httpStatus :: Lens.Lens' CreateStackResponse Prelude.Int
createStackResponse_httpStatus = Lens.lens (\CreateStackResponse' {httpStatus} -> httpStatus) (\s@CreateStackResponse' {} a -> s {httpStatus = a} :: CreateStackResponse)

instance Prelude.NFData CreateStackResponse
