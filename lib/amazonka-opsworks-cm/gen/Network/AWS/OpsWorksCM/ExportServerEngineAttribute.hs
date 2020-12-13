{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.ExportServerEngineAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a specified server engine attribute as a base64-encoded string. For example, you can export user data that you can use in EC2 to associate nodes with a server.
--
-- This operation is synchronous.
-- A @ValidationException@ is raised when parameters of the request are not valid. A @ResourceNotFoundException@ is thrown when the server does not exist. An @InvalidStateException@ is thrown when the server is in any of the following states: CREATING, TERMINATED, FAILED or DELETING.
module Network.AWS.OpsWorksCM.ExportServerEngineAttribute
  ( -- * Creating a request
    ExportServerEngineAttribute (..),
    mkExportServerEngineAttribute,

    -- ** Request lenses
    eseaServerName,
    eseaInputAttributes,
    eseaExportAttributeName,

    -- * Destructuring the response
    ExportServerEngineAttributeResponse (..),
    mkExportServerEngineAttributeResponse,

    -- ** Response lenses
    esearsServerName,
    esearsEngineAttribute,
    esearsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExportServerEngineAttribute' smart constructor.
data ExportServerEngineAttribute = ExportServerEngineAttribute'
  { -- | The name of the server from which you are exporting the attribute.
    serverName :: Lude.Text,
    -- | The list of engine attributes. The list type is @EngineAttribute@ . An @EngineAttribute@ list item is a pair that includes an attribute name and its value. For the @Userdata@ ExportAttributeName, the following are supported engine attribute names.
    --
    --
    --     * __RunList__ In Chef, a list of roles or recipes that are run in the specified order. In Puppet, this parameter is ignored.
    --
    --
    --     * __OrganizationName__ In Chef, an organization name. AWS OpsWorks for Chef Automate always creates the organization @default@ . In Puppet, this parameter is ignored.
    --
    --
    --     * __NodeEnvironment__ In Chef, a node environment (for example, development, staging, or one-box). In Puppet, this parameter is ignored.
    --
    --
    --     * __NodeClientVersion__ In Chef, the version of the Chef engine (three numbers separated by dots, such as 13.8.5). If this attribute is empty, OpsWorks for Chef Automate uses the most current version. In Puppet, this parameter is ignored.
    inputAttributes :: Lude.Maybe [EngineAttribute],
    -- | The name of the export attribute. Currently, the supported export attribute is @Userdata@ . This exports a user data script that includes parameters and values provided in the @InputAttributes@ list.
    exportAttributeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportServerEngineAttribute' with the minimum fields required to make a request.
--
-- * 'serverName' - The name of the server from which you are exporting the attribute.
-- * 'inputAttributes' - The list of engine attributes. The list type is @EngineAttribute@ . An @EngineAttribute@ list item is a pair that includes an attribute name and its value. For the @Userdata@ ExportAttributeName, the following are supported engine attribute names.
--
--
--     * __RunList__ In Chef, a list of roles or recipes that are run in the specified order. In Puppet, this parameter is ignored.
--
--
--     * __OrganizationName__ In Chef, an organization name. AWS OpsWorks for Chef Automate always creates the organization @default@ . In Puppet, this parameter is ignored.
--
--
--     * __NodeEnvironment__ In Chef, a node environment (for example, development, staging, or one-box). In Puppet, this parameter is ignored.
--
--
--     * __NodeClientVersion__ In Chef, the version of the Chef engine (three numbers separated by dots, such as 13.8.5). If this attribute is empty, OpsWorks for Chef Automate uses the most current version. In Puppet, this parameter is ignored.
--
--
-- * 'exportAttributeName' - The name of the export attribute. Currently, the supported export attribute is @Userdata@ . This exports a user data script that includes parameters and values provided in the @InputAttributes@ list.
mkExportServerEngineAttribute ::
  -- | 'serverName'
  Lude.Text ->
  -- | 'exportAttributeName'
  Lude.Text ->
  ExportServerEngineAttribute
mkExportServerEngineAttribute pServerName_ pExportAttributeName_ =
  ExportServerEngineAttribute'
    { serverName = pServerName_,
      inputAttributes = Lude.Nothing,
      exportAttributeName = pExportAttributeName_
    }

-- | The name of the server from which you are exporting the attribute.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eseaServerName :: Lens.Lens' ExportServerEngineAttribute Lude.Text
eseaServerName = Lens.lens (serverName :: ExportServerEngineAttribute -> Lude.Text) (\s a -> s {serverName = a} :: ExportServerEngineAttribute)
{-# DEPRECATED eseaServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The list of engine attributes. The list type is @EngineAttribute@ . An @EngineAttribute@ list item is a pair that includes an attribute name and its value. For the @Userdata@ ExportAttributeName, the following are supported engine attribute names.
--
--
--     * __RunList__ In Chef, a list of roles or recipes that are run in the specified order. In Puppet, this parameter is ignored.
--
--
--     * __OrganizationName__ In Chef, an organization name. AWS OpsWorks for Chef Automate always creates the organization @default@ . In Puppet, this parameter is ignored.
--
--
--     * __NodeEnvironment__ In Chef, a node environment (for example, development, staging, or one-box). In Puppet, this parameter is ignored.
--
--
--     * __NodeClientVersion__ In Chef, the version of the Chef engine (three numbers separated by dots, such as 13.8.5). If this attribute is empty, OpsWorks for Chef Automate uses the most current version. In Puppet, this parameter is ignored.
--
--
--
-- /Note:/ Consider using 'inputAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eseaInputAttributes :: Lens.Lens' ExportServerEngineAttribute (Lude.Maybe [EngineAttribute])
eseaInputAttributes = Lens.lens (inputAttributes :: ExportServerEngineAttribute -> Lude.Maybe [EngineAttribute]) (\s a -> s {inputAttributes = a} :: ExportServerEngineAttribute)
{-# DEPRECATED eseaInputAttributes "Use generic-lens or generic-optics with 'inputAttributes' instead." #-}

-- | The name of the export attribute. Currently, the supported export attribute is @Userdata@ . This exports a user data script that includes parameters and values provided in the @InputAttributes@ list.
--
-- /Note:/ Consider using 'exportAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eseaExportAttributeName :: Lens.Lens' ExportServerEngineAttribute Lude.Text
eseaExportAttributeName = Lens.lens (exportAttributeName :: ExportServerEngineAttribute -> Lude.Text) (\s a -> s {exportAttributeName = a} :: ExportServerEngineAttribute)
{-# DEPRECATED eseaExportAttributeName "Use generic-lens or generic-optics with 'exportAttributeName' instead." #-}

instance Lude.AWSRequest ExportServerEngineAttribute where
  type
    Rs ExportServerEngineAttribute =
      ExportServerEngineAttributeResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          ExportServerEngineAttributeResponse'
            Lude.<$> (x Lude..?> "ServerName")
            Lude.<*> (x Lude..?> "EngineAttribute")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExportServerEngineAttribute where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OpsWorksCM_V2016_11_01.ExportServerEngineAttribute" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExportServerEngineAttribute where
  toJSON ExportServerEngineAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ServerName" Lude..= serverName),
            ("InputAttributes" Lude..=) Lude.<$> inputAttributes,
            Lude.Just ("ExportAttributeName" Lude..= exportAttributeName)
          ]
      )

instance Lude.ToPath ExportServerEngineAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ExportServerEngineAttribute where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkExportServerEngineAttributeResponse' smart constructor.
data ExportServerEngineAttributeResponse = ExportServerEngineAttributeResponse'
  { -- | The server name used in the request.
    serverName :: Lude.Maybe Lude.Text,
    -- | The requested engine attribute pair with attribute name and value.
    engineAttribute :: Lude.Maybe EngineAttribute,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportServerEngineAttributeResponse' with the minimum fields required to make a request.
--
-- * 'serverName' - The server name used in the request.
-- * 'engineAttribute' - The requested engine attribute pair with attribute name and value.
-- * 'responseStatus' - The response status code.
mkExportServerEngineAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExportServerEngineAttributeResponse
mkExportServerEngineAttributeResponse pResponseStatus_ =
  ExportServerEngineAttributeResponse'
    { serverName = Lude.Nothing,
      engineAttribute = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The server name used in the request.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esearsServerName :: Lens.Lens' ExportServerEngineAttributeResponse (Lude.Maybe Lude.Text)
esearsServerName = Lens.lens (serverName :: ExportServerEngineAttributeResponse -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: ExportServerEngineAttributeResponse)
{-# DEPRECATED esearsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The requested engine attribute pair with attribute name and value.
--
-- /Note:/ Consider using 'engineAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esearsEngineAttribute :: Lens.Lens' ExportServerEngineAttributeResponse (Lude.Maybe EngineAttribute)
esearsEngineAttribute = Lens.lens (engineAttribute :: ExportServerEngineAttributeResponse -> Lude.Maybe EngineAttribute) (\s a -> s {engineAttribute = a} :: ExportServerEngineAttributeResponse)
{-# DEPRECATED esearsEngineAttribute "Use generic-lens or generic-optics with 'engineAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esearsResponseStatus :: Lens.Lens' ExportServerEngineAttributeResponse Lude.Int
esearsResponseStatus = Lens.lens (responseStatus :: ExportServerEngineAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExportServerEngineAttributeResponse)
{-# DEPRECATED esearsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
