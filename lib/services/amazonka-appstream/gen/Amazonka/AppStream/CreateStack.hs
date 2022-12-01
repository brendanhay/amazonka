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
-- Module      : Amazonka.AppStream.CreateStack
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack to start streaming applications to users. A stack
-- consists of an associated fleet, user access policies, and storage
-- configurations.
module Amazonka.AppStream.CreateStack
  ( -- * Creating a Request
    CreateStack (..),
    newCreateStack,

    -- * Request Lenses
    createStack_tags,
    createStack_storageConnectors,
    createStack_embedHostDomains,
    createStack_applicationSettings,
    createStack_displayName,
    createStack_accessEndpoints,
    createStack_description,
    createStack_redirectURL,
    createStack_streamingExperienceSettings,
    createStack_feedbackURL,
    createStack_userSettings,
    createStack_name,

    -- * Destructuring the Response
    CreateStackResponse (..),
    newCreateStackResponse,

    -- * Response Lenses
    createStackResponse_stack,
    createStackResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStack' smart constructor.
data CreateStack = CreateStack'
  { -- | The tags to associate with the stack. A tag is a key-value pair, and the
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
    -- | The domains where AppStream 2.0 streaming sessions can be embedded in an
    -- iframe. You must approve the domains that you want to host embedded
    -- AppStream 2.0 streaming sessions.
    embedHostDomains :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The persistent application settings for users of a stack. When these
    -- settings are enabled, changes that users make to applications and
    -- Windows settings are automatically saved after each session and applied
    -- to the next session.
    applicationSettings :: Prelude.Maybe ApplicationSettings,
    -- | The stack name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The list of interface VPC endpoint (interface endpoint) objects. Users
    -- of the stack can connect to AppStream 2.0 only through the specified
    -- endpoints.
    accessEndpoints :: Prelude.Maybe (Prelude.NonEmpty AccessEndpoint),
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The URL that users are redirected to after their streaming session ends.
    redirectURL :: Prelude.Maybe Prelude.Text,
    -- | The streaming protocol you want your stack to prefer. This can be UDP or
    -- TCP. Currently, UDP is only supported in the Windows native client.
    streamingExperienceSettings :: Prelude.Maybe StreamingExperienceSettings,
    -- | The URL that users are redirected to after they click the Send Feedback
    -- link. If no URL is specified, no Send Feedback link is displayed.
    feedbackURL :: Prelude.Maybe Prelude.Text,
    -- | The actions that are enabled or disabled for users during their
    -- streaming sessions. By default, these actions are enabled.
    userSettings :: Prelude.Maybe (Prelude.NonEmpty UserSetting),
    -- | The name of the stack.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'embedHostDomains', 'createStack_embedHostDomains' - The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
--
-- 'applicationSettings', 'createStack_applicationSettings' - The persistent application settings for users of a stack. When these
-- settings are enabled, changes that users make to applications and
-- Windows settings are automatically saved after each session and applied
-- to the next session.
--
-- 'displayName', 'createStack_displayName' - The stack name to display.
--
-- 'accessEndpoints', 'createStack_accessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects. Users
-- of the stack can connect to AppStream 2.0 only through the specified
-- endpoints.
--
-- 'description', 'createStack_description' - The description to display.
--
-- 'redirectURL', 'createStack_redirectURL' - The URL that users are redirected to after their streaming session ends.
--
-- 'streamingExperienceSettings', 'createStack_streamingExperienceSettings' - The streaming protocol you want your stack to prefer. This can be UDP or
-- TCP. Currently, UDP is only supported in the Windows native client.
--
-- 'feedbackURL', 'createStack_feedbackURL' - The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
--
-- 'userSettings', 'createStack_userSettings' - The actions that are enabled or disabled for users during their
-- streaming sessions. By default, these actions are enabled.
--
-- 'name', 'createStack_name' - The name of the stack.
newCreateStack ::
  -- | 'name'
  Prelude.Text ->
  CreateStack
newCreateStack pName_ =
  CreateStack'
    { tags = Prelude.Nothing,
      storageConnectors = Prelude.Nothing,
      embedHostDomains = Prelude.Nothing,
      applicationSettings = Prelude.Nothing,
      displayName = Prelude.Nothing,
      accessEndpoints = Prelude.Nothing,
      description = Prelude.Nothing,
      redirectURL = Prelude.Nothing,
      streamingExperienceSettings = Prelude.Nothing,
      feedbackURL = Prelude.Nothing,
      userSettings = Prelude.Nothing,
      name = pName_
    }

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
createStack_tags = Lens.lens (\CreateStack' {tags} -> tags) (\s@CreateStack' {} a -> s {tags = a} :: CreateStack) Prelude.. Lens.mapping Lens.coerced

-- | The storage connectors to enable.
createStack_storageConnectors :: Lens.Lens' CreateStack (Prelude.Maybe [StorageConnector])
createStack_storageConnectors = Lens.lens (\CreateStack' {storageConnectors} -> storageConnectors) (\s@CreateStack' {} a -> s {storageConnectors = a} :: CreateStack) Prelude.. Lens.mapping Lens.coerced

-- | The domains where AppStream 2.0 streaming sessions can be embedded in an
-- iframe. You must approve the domains that you want to host embedded
-- AppStream 2.0 streaming sessions.
createStack_embedHostDomains :: Lens.Lens' CreateStack (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createStack_embedHostDomains = Lens.lens (\CreateStack' {embedHostDomains} -> embedHostDomains) (\s@CreateStack' {} a -> s {embedHostDomains = a} :: CreateStack) Prelude.. Lens.mapping Lens.coerced

-- | The persistent application settings for users of a stack. When these
-- settings are enabled, changes that users make to applications and
-- Windows settings are automatically saved after each session and applied
-- to the next session.
createStack_applicationSettings :: Lens.Lens' CreateStack (Prelude.Maybe ApplicationSettings)
createStack_applicationSettings = Lens.lens (\CreateStack' {applicationSettings} -> applicationSettings) (\s@CreateStack' {} a -> s {applicationSettings = a} :: CreateStack)

-- | The stack name to display.
createStack_displayName :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_displayName = Lens.lens (\CreateStack' {displayName} -> displayName) (\s@CreateStack' {} a -> s {displayName = a} :: CreateStack)

-- | The list of interface VPC endpoint (interface endpoint) objects. Users
-- of the stack can connect to AppStream 2.0 only through the specified
-- endpoints.
createStack_accessEndpoints :: Lens.Lens' CreateStack (Prelude.Maybe (Prelude.NonEmpty AccessEndpoint))
createStack_accessEndpoints = Lens.lens (\CreateStack' {accessEndpoints} -> accessEndpoints) (\s@CreateStack' {} a -> s {accessEndpoints = a} :: CreateStack) Prelude.. Lens.mapping Lens.coerced

-- | The description to display.
createStack_description :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_description = Lens.lens (\CreateStack' {description} -> description) (\s@CreateStack' {} a -> s {description = a} :: CreateStack)

-- | The URL that users are redirected to after their streaming session ends.
createStack_redirectURL :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_redirectURL = Lens.lens (\CreateStack' {redirectURL} -> redirectURL) (\s@CreateStack' {} a -> s {redirectURL = a} :: CreateStack)

-- | The streaming protocol you want your stack to prefer. This can be UDP or
-- TCP. Currently, UDP is only supported in the Windows native client.
createStack_streamingExperienceSettings :: Lens.Lens' CreateStack (Prelude.Maybe StreamingExperienceSettings)
createStack_streamingExperienceSettings = Lens.lens (\CreateStack' {streamingExperienceSettings} -> streamingExperienceSettings) (\s@CreateStack' {} a -> s {streamingExperienceSettings = a} :: CreateStack)

-- | The URL that users are redirected to after they click the Send Feedback
-- link. If no URL is specified, no Send Feedback link is displayed.
createStack_feedbackURL :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_feedbackURL = Lens.lens (\CreateStack' {feedbackURL} -> feedbackURL) (\s@CreateStack' {} a -> s {feedbackURL = a} :: CreateStack)

-- | The actions that are enabled or disabled for users during their
-- streaming sessions. By default, these actions are enabled.
createStack_userSettings :: Lens.Lens' CreateStack (Prelude.Maybe (Prelude.NonEmpty UserSetting))
createStack_userSettings = Lens.lens (\CreateStack' {userSettings} -> userSettings) (\s@CreateStack' {} a -> s {userSettings = a} :: CreateStack) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stack.
createStack_name :: Lens.Lens' CreateStack Prelude.Text
createStack_name = Lens.lens (\CreateStack' {name} -> name) (\s@CreateStack' {} a -> s {name = a} :: CreateStack)

instance Core.AWSRequest CreateStack where
  type AWSResponse CreateStack = CreateStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStackResponse'
            Prelude.<$> (x Core..?> "Stack")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStack where
  hashWithSalt _salt CreateStack' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` storageConnectors
      `Prelude.hashWithSalt` embedHostDomains
      `Prelude.hashWithSalt` applicationSettings
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` accessEndpoints
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` redirectURL
      `Prelude.hashWithSalt` streamingExperienceSettings
      `Prelude.hashWithSalt` feedbackURL
      `Prelude.hashWithSalt` userSettings
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateStack where
  rnf CreateStack' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf storageConnectors
      `Prelude.seq` Prelude.rnf embedHostDomains
      `Prelude.seq` Prelude.rnf applicationSettings
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf accessEndpoints
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf redirectURL
      `Prelude.seq` Prelude.rnf streamingExperienceSettings
      `Prelude.seq` Prelude.rnf feedbackURL
      `Prelude.seq` Prelude.rnf userSettings
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateStack where
  toJSON CreateStack' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("StorageConnectors" Core..=)
              Prelude.<$> storageConnectors,
            ("EmbedHostDomains" Core..=)
              Prelude.<$> embedHostDomains,
            ("ApplicationSettings" Core..=)
              Prelude.<$> applicationSettings,
            ("DisplayName" Core..=) Prelude.<$> displayName,
            ("AccessEndpoints" Core..=)
              Prelude.<$> accessEndpoints,
            ("Description" Core..=) Prelude.<$> description,
            ("RedirectURL" Core..=) Prelude.<$> redirectURL,
            ("StreamingExperienceSettings" Core..=)
              Prelude.<$> streamingExperienceSettings,
            ("FeedbackURL" Core..=) Prelude.<$> feedbackURL,
            ("UserSettings" Core..=) Prelude.<$> userSettings,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateStack where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { -- | Information about the stack.
    stack :: Prelude.Maybe Stack,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateStackResponse where
  rnf CreateStackResponse' {..} =
    Prelude.rnf stack
      `Prelude.seq` Prelude.rnf httpStatus
