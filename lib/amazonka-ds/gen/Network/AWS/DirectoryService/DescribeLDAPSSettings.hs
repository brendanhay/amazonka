{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeLDAPSSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of LDAP security for the specified directory.
module Network.AWS.DirectoryService.DescribeLDAPSSettings
  ( -- * Creating a request
    DescribeLDAPSSettings (..),
    mkDescribeLDAPSSettings,

    -- ** Request lenses
    dldapssNextToken,
    dldapssLimit,
    dldapssType,
    dldapssDirectoryId,

    -- * Destructuring the response
    DescribeLDAPSSettingsResponse (..),
    mkDescribeLDAPSSettingsResponse,

    -- ** Response lenses
    dldapssrsLDAPSSettingsInfo,
    dldapssrsNextToken,
    dldapssrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLDAPSSettings' smart constructor.
data DescribeLDAPSSettings = DescribeLDAPSSettings'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    type' :: Lude.Maybe LDAPSType,
    directoryId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLDAPSSettings' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'limit' - Specifies the number of items that should be displayed on one page.
-- * 'nextToken' - The type of next token used for pagination.
-- * 'type'' - The type of LDAP security to enable. Currently only the value @Client@ is supported.
mkDescribeLDAPSSettings ::
  -- | 'directoryId'
  Lude.Text ->
  DescribeLDAPSSettings
mkDescribeLDAPSSettings pDirectoryId_ =
  DescribeLDAPSSettings'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      type' = Lude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The type of next token used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssNextToken :: Lens.Lens' DescribeLDAPSSettings (Lude.Maybe Lude.Text)
dldapssNextToken = Lens.lens (nextToken :: DescribeLDAPSSettings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLDAPSSettings)
{-# DEPRECATED dldapssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the number of items that should be displayed on one page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssLimit :: Lens.Lens' DescribeLDAPSSettings (Lude.Maybe Lude.Natural)
dldapssLimit = Lens.lens (limit :: DescribeLDAPSSettings -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeLDAPSSettings)
{-# DEPRECATED dldapssLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssType :: Lens.Lens' DescribeLDAPSSettings (Lude.Maybe LDAPSType)
dldapssType = Lens.lens (type' :: DescribeLDAPSSettings -> Lude.Maybe LDAPSType) (\s a -> s {type' = a} :: DescribeLDAPSSettings)
{-# DEPRECATED dldapssType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssDirectoryId :: Lens.Lens' DescribeLDAPSSettings Lude.Text
dldapssDirectoryId = Lens.lens (directoryId :: DescribeLDAPSSettings -> Lude.Text) (\s a -> s {directoryId = a} :: DescribeLDAPSSettings)
{-# DEPRECATED dldapssDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest DescribeLDAPSSettings where
  type Rs DescribeLDAPSSettings = DescribeLDAPSSettingsResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLDAPSSettingsResponse'
            Lude.<$> (x Lude..?> "LDAPSSettingsInfo" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLDAPSSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DescribeLDAPSSettings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLDAPSSettings where
  toJSON DescribeLDAPSSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            ("Type" Lude..=) Lude.<$> type',
            Lude.Just ("DirectoryId" Lude..= directoryId)
          ]
      )

instance Lude.ToPath DescribeLDAPSSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLDAPSSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLDAPSSettingsResponse' smart constructor.
data DescribeLDAPSSettingsResponse = DescribeLDAPSSettingsResponse'
  { lDAPSSettingsInfo ::
      Lude.Maybe [LDAPSSettingInfo],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLDAPSSettingsResponse' with the minimum fields required to make a request.
--
-- * 'lDAPSSettingsInfo' - Information about LDAP security for the specified directory, including status of enablement, state last updated date time, and the reason for the state.
-- * 'nextToken' - The next token used to retrieve the LDAPS settings if the number of setting types exceeds page limit and there is another page.
-- * 'responseStatus' - The response status code.
mkDescribeLDAPSSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLDAPSSettingsResponse
mkDescribeLDAPSSettingsResponse pResponseStatus_ =
  DescribeLDAPSSettingsResponse'
    { lDAPSSettingsInfo = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about LDAP security for the specified directory, including status of enablement, state last updated date time, and the reason for the state.
--
-- /Note:/ Consider using 'lDAPSSettingsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssrsLDAPSSettingsInfo :: Lens.Lens' DescribeLDAPSSettingsResponse (Lude.Maybe [LDAPSSettingInfo])
dldapssrsLDAPSSettingsInfo = Lens.lens (lDAPSSettingsInfo :: DescribeLDAPSSettingsResponse -> Lude.Maybe [LDAPSSettingInfo]) (\s a -> s {lDAPSSettingsInfo = a} :: DescribeLDAPSSettingsResponse)
{-# DEPRECATED dldapssrsLDAPSSettingsInfo "Use generic-lens or generic-optics with 'lDAPSSettingsInfo' instead." #-}

-- | The next token used to retrieve the LDAPS settings if the number of setting types exceeds page limit and there is another page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssrsNextToken :: Lens.Lens' DescribeLDAPSSettingsResponse (Lude.Maybe Lude.Text)
dldapssrsNextToken = Lens.lens (nextToken :: DescribeLDAPSSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLDAPSSettingsResponse)
{-# DEPRECATED dldapssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapssrsResponseStatus :: Lens.Lens' DescribeLDAPSSettingsResponse Lude.Int
dldapssrsResponseStatus = Lens.lens (responseStatus :: DescribeLDAPSSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLDAPSSettingsResponse)
{-# DEPRECATED dldapssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
