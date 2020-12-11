{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.UpdateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified configuration.
module Network.AWS.MQ.UpdateConfiguration
  ( -- * Creating a request
    UpdateConfiguration (..),
    mkUpdateConfiguration,

    -- ** Request lenses
    ucData,
    ucDescription,
    ucConfigurationId,

    -- * Destructuring the response
    UpdateConfigurationResponse (..),
    mkUpdateConfigurationResponse,

    -- ** Response lenses
    ucrsARN,
    ucrsLatestRevision,
    ucrsCreated,
    ucrsWarnings,
    ucrsName,
    ucrsId,
    ucrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Updates the specified configuration.
--
-- /See:/ 'mkUpdateConfiguration' smart constructor.
data UpdateConfiguration = UpdateConfiguration'
  { data' ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    configurationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfiguration' with the minimum fields required to make a request.
--
-- * 'configurationId' - The unique ID that Amazon MQ generates for the configuration.
-- * 'data'' - Required. The base64-encoded XML configuration.
-- * 'description' - The description of the configuration.
mkUpdateConfiguration ::
  -- | 'configurationId'
  Lude.Text ->
  UpdateConfiguration
mkUpdateConfiguration pConfigurationId_ =
  UpdateConfiguration'
    { data' = Lude.Nothing,
      description = Lude.Nothing,
      configurationId = pConfigurationId_
    }

-- | Required. The base64-encoded XML configuration.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucData :: Lens.Lens' UpdateConfiguration (Lude.Maybe Lude.Text)
ucData = Lens.lens (data' :: UpdateConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {data' = a} :: UpdateConfiguration)
{-# DEPRECATED ucData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The description of the configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateConfiguration (Lude.Maybe Lude.Text)
ucDescription = Lens.lens (description :: UpdateConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateConfiguration)
{-# DEPRECATED ucDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucConfigurationId :: Lens.Lens' UpdateConfiguration Lude.Text
ucConfigurationId = Lens.lens (configurationId :: UpdateConfiguration -> Lude.Text) (\s a -> s {configurationId = a} :: UpdateConfiguration)
{-# DEPRECATED ucConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

instance Lude.AWSRequest UpdateConfiguration where
  type Rs UpdateConfiguration = UpdateConfigurationResponse
  request = Req.putJSON mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateConfigurationResponse'
            Lude.<$> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "latestRevision")
            Lude.<*> (x Lude..?> "created")
            Lude.<*> (x Lude..?> "warnings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateConfiguration where
  toJSON UpdateConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("data" Lude..=) Lude.<$> data',
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateConfiguration where
  toPath UpdateConfiguration' {..} =
    Lude.mconcat ["/v1/configurations/", Lude.toBS configurationId]

instance Lude.ToQuery UpdateConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateConfigurationResponse' smart constructor.
data UpdateConfigurationResponse = UpdateConfigurationResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    latestRevision ::
      Lude.Maybe ConfigurationRevision,
    created ::
      Lude.Maybe Lude.Timestamp,
    warnings ::
      Lude.Maybe [SanitizationWarning],
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'arn' - Required. The Amazon Resource Name (ARN) of the configuration.
-- * 'created' - Required. The date and time of the configuration.
-- * 'id' - Required. The unique ID that Amazon MQ generates for the configuration.
-- * 'latestRevision' - The latest revision of the configuration.
-- * 'name' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
-- * 'responseStatus' - The response status code.
-- * 'warnings' - The list of the first 20 warnings about the configuration XML elements or attributes that were sanitized.
mkUpdateConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConfigurationResponse
mkUpdateConfigurationResponse pResponseStatus_ =
  UpdateConfigurationResponse'
    { arn = Lude.Nothing,
      latestRevision = Lude.Nothing,
      created = Lude.Nothing,
      warnings = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Required. The Amazon Resource Name (ARN) of the configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsARN :: Lens.Lens' UpdateConfigurationResponse (Lude.Maybe Lude.Text)
ucrsARN = Lens.lens (arn :: UpdateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UpdateConfigurationResponse)
{-# DEPRECATED ucrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The latest revision of the configuration.
--
-- /Note:/ Consider using 'latestRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsLatestRevision :: Lens.Lens' UpdateConfigurationResponse (Lude.Maybe ConfigurationRevision)
ucrsLatestRevision = Lens.lens (latestRevision :: UpdateConfigurationResponse -> Lude.Maybe ConfigurationRevision) (\s a -> s {latestRevision = a} :: UpdateConfigurationResponse)
{-# DEPRECATED ucrsLatestRevision "Use generic-lens or generic-optics with 'latestRevision' instead." #-}

-- | Required. The date and time of the configuration.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsCreated :: Lens.Lens' UpdateConfigurationResponse (Lude.Maybe Lude.Timestamp)
ucrsCreated = Lens.lens (created :: UpdateConfigurationResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: UpdateConfigurationResponse)
{-# DEPRECATED ucrsCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The list of the first 20 warnings about the configuration XML elements or attributes that were sanitized.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsWarnings :: Lens.Lens' UpdateConfigurationResponse (Lude.Maybe [SanitizationWarning])
ucrsWarnings = Lens.lens (warnings :: UpdateConfigurationResponse -> Lude.Maybe [SanitizationWarning]) (\s a -> s {warnings = a} :: UpdateConfigurationResponse)
{-# DEPRECATED ucrsWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsName :: Lens.Lens' UpdateConfigurationResponse (Lude.Maybe Lude.Text)
ucrsName = Lens.lens (name :: UpdateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateConfigurationResponse)
{-# DEPRECATED ucrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsId :: Lens.Lens' UpdateConfigurationResponse (Lude.Maybe Lude.Text)
ucrsId = Lens.lens (id :: UpdateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UpdateConfigurationResponse)
{-# DEPRECATED ucrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateConfigurationResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConfigurationResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
