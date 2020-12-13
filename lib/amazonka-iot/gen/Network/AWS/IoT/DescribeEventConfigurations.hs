{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeEventConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes event configurations.
module Network.AWS.IoT.DescribeEventConfigurations
  ( -- * Creating a request
    DescribeEventConfigurations (..),
    mkDescribeEventConfigurations,

    -- * Destructuring the response
    DescribeEventConfigurationsResponse (..),
    mkDescribeEventConfigurationsResponse,

    -- ** Response lenses
    decrsLastModifiedDate,
    decrsEventConfigurations,
    decrsCreationDate,
    decrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEventConfigurations' smart constructor.
data DescribeEventConfigurations = DescribeEventConfigurations'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventConfigurations' with the minimum fields required to make a request.
mkDescribeEventConfigurations ::
  DescribeEventConfigurations
mkDescribeEventConfigurations = DescribeEventConfigurations'

instance Lude.AWSRequest DescribeEventConfigurations where
  type
    Rs DescribeEventConfigurations =
      DescribeEventConfigurationsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventConfigurationsResponse'
            Lude.<$> (x Lude..?> "lastModifiedDate")
            Lude.<*> (x Lude..?> "eventConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventConfigurations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEventConfigurations where
  toPath = Lude.const "/event-configurations"

instance Lude.ToQuery DescribeEventConfigurations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEventConfigurationsResponse' smart constructor.
data DescribeEventConfigurationsResponse = DescribeEventConfigurationsResponse'
  { -- | The date the event configurations were last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The event configurations.
    eventConfigurations :: Lude.Maybe (Lude.HashMap EventType (Configuration)),
    -- | The creation date of the event configuration.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - The date the event configurations were last modified.
-- * 'eventConfigurations' - The event configurations.
-- * 'creationDate' - The creation date of the event configuration.
-- * 'responseStatus' - The response status code.
mkDescribeEventConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventConfigurationsResponse
mkDescribeEventConfigurationsResponse pResponseStatus_ =
  DescribeEventConfigurationsResponse'
    { lastModifiedDate =
        Lude.Nothing,
      eventConfigurations = Lude.Nothing,
      creationDate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date the event configurations were last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsLastModifiedDate :: Lens.Lens' DescribeEventConfigurationsResponse (Lude.Maybe Lude.Timestamp)
decrsLastModifiedDate = Lens.lens (lastModifiedDate :: DescribeEventConfigurationsResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: DescribeEventConfigurationsResponse)
{-# DEPRECATED decrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The event configurations.
--
-- /Note:/ Consider using 'eventConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsEventConfigurations :: Lens.Lens' DescribeEventConfigurationsResponse (Lude.Maybe (Lude.HashMap EventType (Configuration)))
decrsEventConfigurations = Lens.lens (eventConfigurations :: DescribeEventConfigurationsResponse -> Lude.Maybe (Lude.HashMap EventType (Configuration))) (\s a -> s {eventConfigurations = a} :: DescribeEventConfigurationsResponse)
{-# DEPRECATED decrsEventConfigurations "Use generic-lens or generic-optics with 'eventConfigurations' instead." #-}

-- | The creation date of the event configuration.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsCreationDate :: Lens.Lens' DescribeEventConfigurationsResponse (Lude.Maybe Lude.Timestamp)
decrsCreationDate = Lens.lens (creationDate :: DescribeEventConfigurationsResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeEventConfigurationsResponse)
{-# DEPRECATED decrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decrsResponseStatus :: Lens.Lens' DescribeEventConfigurationsResponse Lude.Int
decrsResponseStatus = Lens.lens (responseStatus :: DescribeEventConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventConfigurationsResponse)
{-# DEPRECATED decrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
