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
-- Module      : Network.AWS.OpsWorksCM.ExportServerEngineAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a specified server engine attribute as a base64-encoded string.
-- For example, you can export user data that you can use in EC2 to
-- associate nodes with a server.
--
-- This operation is synchronous.
--
-- A @ValidationException@ is raised when parameters of the request are not
-- valid. A @ResourceNotFoundException@ is thrown when the server does not
-- exist. An @InvalidStateException@ is thrown when the server is in any of
-- the following states: CREATING, TERMINATED, FAILED or DELETING.
module Network.AWS.OpsWorksCM.ExportServerEngineAttribute
  ( -- * Creating a Request
    ExportServerEngineAttribute (..),
    newExportServerEngineAttribute,

    -- * Request Lenses
    exportServerEngineAttribute_inputAttributes,
    exportServerEngineAttribute_exportAttributeName,
    exportServerEngineAttribute_serverName,

    -- * Destructuring the Response
    ExportServerEngineAttributeResponse (..),
    newExportServerEngineAttributeResponse,

    -- * Response Lenses
    exportServerEngineAttributeResponse_engineAttribute,
    exportServerEngineAttributeResponse_serverName,
    exportServerEngineAttributeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExportServerEngineAttribute' smart constructor.
data ExportServerEngineAttribute = ExportServerEngineAttribute'
  { -- | The list of engine attributes. The list type is @EngineAttribute@. An
    -- @EngineAttribute@ list item is a pair that includes an attribute name
    -- and its value. For the @Userdata@ ExportAttributeName, the following are
    -- supported engine attribute names.
    --
    -- -   __RunList__ In Chef, a list of roles or recipes that are run in the
    --     specified order. In Puppet, this parameter is ignored.
    --
    -- -   __OrganizationName__ In Chef, an organization name. AWS OpsWorks for
    --     Chef Automate always creates the organization @default@. In Puppet,
    --     this parameter is ignored.
    --
    -- -   __NodeEnvironment__ In Chef, a node environment (for example,
    --     development, staging, or one-box). In Puppet, this parameter is
    --     ignored.
    --
    -- -   __NodeClientVersion__ In Chef, the version of the Chef engine (three
    --     numbers separated by dots, such as 13.8.5). If this attribute is
    --     empty, OpsWorks for Chef Automate uses the most current version. In
    --     Puppet, this parameter is ignored.
    inputAttributes :: Prelude.Maybe [EngineAttribute],
    -- | The name of the export attribute. Currently, the supported export
    -- attribute is @Userdata@. This exports a user data script that includes
    -- parameters and values provided in the @InputAttributes@ list.
    exportAttributeName :: Prelude.Text,
    -- | The name of the server from which you are exporting the attribute.
    serverName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportServerEngineAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputAttributes', 'exportServerEngineAttribute_inputAttributes' - The list of engine attributes. The list type is @EngineAttribute@. An
-- @EngineAttribute@ list item is a pair that includes an attribute name
-- and its value. For the @Userdata@ ExportAttributeName, the following are
-- supported engine attribute names.
--
-- -   __RunList__ In Chef, a list of roles or recipes that are run in the
--     specified order. In Puppet, this parameter is ignored.
--
-- -   __OrganizationName__ In Chef, an organization name. AWS OpsWorks for
--     Chef Automate always creates the organization @default@. In Puppet,
--     this parameter is ignored.
--
-- -   __NodeEnvironment__ In Chef, a node environment (for example,
--     development, staging, or one-box). In Puppet, this parameter is
--     ignored.
--
-- -   __NodeClientVersion__ In Chef, the version of the Chef engine (three
--     numbers separated by dots, such as 13.8.5). If this attribute is
--     empty, OpsWorks for Chef Automate uses the most current version. In
--     Puppet, this parameter is ignored.
--
-- 'exportAttributeName', 'exportServerEngineAttribute_exportAttributeName' - The name of the export attribute. Currently, the supported export
-- attribute is @Userdata@. This exports a user data script that includes
-- parameters and values provided in the @InputAttributes@ list.
--
-- 'serverName', 'exportServerEngineAttribute_serverName' - The name of the server from which you are exporting the attribute.
newExportServerEngineAttribute ::
  -- | 'exportAttributeName'
  Prelude.Text ->
  -- | 'serverName'
  Prelude.Text ->
  ExportServerEngineAttribute
newExportServerEngineAttribute
  pExportAttributeName_
  pServerName_ =
    ExportServerEngineAttribute'
      { inputAttributes =
          Prelude.Nothing,
        exportAttributeName = pExportAttributeName_,
        serverName = pServerName_
      }

-- | The list of engine attributes. The list type is @EngineAttribute@. An
-- @EngineAttribute@ list item is a pair that includes an attribute name
-- and its value. For the @Userdata@ ExportAttributeName, the following are
-- supported engine attribute names.
--
-- -   __RunList__ In Chef, a list of roles or recipes that are run in the
--     specified order. In Puppet, this parameter is ignored.
--
-- -   __OrganizationName__ In Chef, an organization name. AWS OpsWorks for
--     Chef Automate always creates the organization @default@. In Puppet,
--     this parameter is ignored.
--
-- -   __NodeEnvironment__ In Chef, a node environment (for example,
--     development, staging, or one-box). In Puppet, this parameter is
--     ignored.
--
-- -   __NodeClientVersion__ In Chef, the version of the Chef engine (three
--     numbers separated by dots, such as 13.8.5). If this attribute is
--     empty, OpsWorks for Chef Automate uses the most current version. In
--     Puppet, this parameter is ignored.
exportServerEngineAttribute_inputAttributes :: Lens.Lens' ExportServerEngineAttribute (Prelude.Maybe [EngineAttribute])
exportServerEngineAttribute_inputAttributes = Lens.lens (\ExportServerEngineAttribute' {inputAttributes} -> inputAttributes) (\s@ExportServerEngineAttribute' {} a -> s {inputAttributes = a} :: ExportServerEngineAttribute) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the export attribute. Currently, the supported export
-- attribute is @Userdata@. This exports a user data script that includes
-- parameters and values provided in the @InputAttributes@ list.
exportServerEngineAttribute_exportAttributeName :: Lens.Lens' ExportServerEngineAttribute Prelude.Text
exportServerEngineAttribute_exportAttributeName = Lens.lens (\ExportServerEngineAttribute' {exportAttributeName} -> exportAttributeName) (\s@ExportServerEngineAttribute' {} a -> s {exportAttributeName = a} :: ExportServerEngineAttribute)

-- | The name of the server from which you are exporting the attribute.
exportServerEngineAttribute_serverName :: Lens.Lens' ExportServerEngineAttribute Prelude.Text
exportServerEngineAttribute_serverName = Lens.lens (\ExportServerEngineAttribute' {serverName} -> serverName) (\s@ExportServerEngineAttribute' {} a -> s {serverName = a} :: ExportServerEngineAttribute)

instance Core.AWSRequest ExportServerEngineAttribute where
  type
    AWSResponse ExportServerEngineAttribute =
      ExportServerEngineAttributeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportServerEngineAttributeResponse'
            Prelude.<$> (x Core..?> "EngineAttribute")
            Prelude.<*> (x Core..?> "ServerName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportServerEngineAttribute

instance Prelude.NFData ExportServerEngineAttribute

instance Core.ToHeaders ExportServerEngineAttribute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.ExportServerEngineAttribute" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ExportServerEngineAttribute where
  toJSON ExportServerEngineAttribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InputAttributes" Core..=)
              Prelude.<$> inputAttributes,
            Prelude.Just
              ("ExportAttributeName" Core..= exportAttributeName),
            Prelude.Just ("ServerName" Core..= serverName)
          ]
      )

instance Core.ToPath ExportServerEngineAttribute where
  toPath = Prelude.const "/"

instance Core.ToQuery ExportServerEngineAttribute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportServerEngineAttributeResponse' smart constructor.
data ExportServerEngineAttributeResponse = ExportServerEngineAttributeResponse'
  { -- | The requested engine attribute pair with attribute name and value.
    engineAttribute :: Prelude.Maybe EngineAttribute,
    -- | The server name used in the request.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportServerEngineAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineAttribute', 'exportServerEngineAttributeResponse_engineAttribute' - The requested engine attribute pair with attribute name and value.
--
-- 'serverName', 'exportServerEngineAttributeResponse_serverName' - The server name used in the request.
--
-- 'httpStatus', 'exportServerEngineAttributeResponse_httpStatus' - The response's http status code.
newExportServerEngineAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportServerEngineAttributeResponse
newExportServerEngineAttributeResponse pHttpStatus_ =
  ExportServerEngineAttributeResponse'
    { engineAttribute =
        Prelude.Nothing,
      serverName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested engine attribute pair with attribute name and value.
exportServerEngineAttributeResponse_engineAttribute :: Lens.Lens' ExportServerEngineAttributeResponse (Prelude.Maybe EngineAttribute)
exportServerEngineAttributeResponse_engineAttribute = Lens.lens (\ExportServerEngineAttributeResponse' {engineAttribute} -> engineAttribute) (\s@ExportServerEngineAttributeResponse' {} a -> s {engineAttribute = a} :: ExportServerEngineAttributeResponse)

-- | The server name used in the request.
exportServerEngineAttributeResponse_serverName :: Lens.Lens' ExportServerEngineAttributeResponse (Prelude.Maybe Prelude.Text)
exportServerEngineAttributeResponse_serverName = Lens.lens (\ExportServerEngineAttributeResponse' {serverName} -> serverName) (\s@ExportServerEngineAttributeResponse' {} a -> s {serverName = a} :: ExportServerEngineAttributeResponse)

-- | The response's http status code.
exportServerEngineAttributeResponse_httpStatus :: Lens.Lens' ExportServerEngineAttributeResponse Prelude.Int
exportServerEngineAttributeResponse_httpStatus = Lens.lens (\ExportServerEngineAttributeResponse' {httpStatus} -> httpStatus) (\s@ExportServerEngineAttributeResponse' {} a -> s {httpStatus = a} :: ExportServerEngineAttributeResponse)

instance
  Prelude.NFData
    ExportServerEngineAttributeResponse
