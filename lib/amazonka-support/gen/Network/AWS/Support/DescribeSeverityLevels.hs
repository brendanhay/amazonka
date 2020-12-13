{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeSeverityLevels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of severity levels that you can assign to an AWS Support case. The severity level for a case is also a field in the 'CaseDetails' data type that you include for a 'CreateCase' request.
module Network.AWS.Support.DescribeSeverityLevels
  ( -- * Creating a request
    DescribeSeverityLevels (..),
    mkDescribeSeverityLevels,

    -- ** Request lenses
    dslLanguage,

    -- * Destructuring the response
    DescribeSeverityLevelsResponse (..),
    mkDescribeSeverityLevelsResponse,

    -- ** Response lenses
    dslrsSeverityLevels,
    dslrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkDescribeSeverityLevels' smart constructor.
newtype DescribeSeverityLevels = DescribeSeverityLevels'
  { -- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
    language :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSeverityLevels' with the minimum fields required to make a request.
--
-- * 'language' - The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
mkDescribeSeverityLevels ::
  DescribeSeverityLevels
mkDescribeSeverityLevels =
  DescribeSeverityLevels' {language = Lude.Nothing}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslLanguage :: Lens.Lens' DescribeSeverityLevels (Lude.Maybe Lude.Text)
dslLanguage = Lens.lens (language :: DescribeSeverityLevels -> Lude.Maybe Lude.Text) (\s a -> s {language = a} :: DescribeSeverityLevels)
{-# DEPRECATED dslLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Lude.AWSRequest DescribeSeverityLevels where
  type Rs DescribeSeverityLevels = DescribeSeverityLevelsResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSeverityLevelsResponse'
            Lude.<$> (x Lude..?> "severityLevels" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSeverityLevels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSSupport_20130415.DescribeSeverityLevels" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSeverityLevels where
  toJSON DescribeSeverityLevels' {..} =
    Lude.object
      (Lude.catMaybes [("language" Lude..=) Lude.<$> language])

instance Lude.ToPath DescribeSeverityLevels where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSeverityLevels where
  toQuery = Lude.const Lude.mempty

-- | The list of severity levels returned by the 'DescribeSeverityLevels' operation.
--
-- /See:/ 'mkDescribeSeverityLevelsResponse' smart constructor.
data DescribeSeverityLevelsResponse = DescribeSeverityLevelsResponse'
  { -- | The available severity levels for the support case. Available severity levels are defined by your service level agreement with AWS.
    severityLevels :: Lude.Maybe [SeverityLevel],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSeverityLevelsResponse' with the minimum fields required to make a request.
--
-- * 'severityLevels' - The available severity levels for the support case. Available severity levels are defined by your service level agreement with AWS.
-- * 'responseStatus' - The response status code.
mkDescribeSeverityLevelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSeverityLevelsResponse
mkDescribeSeverityLevelsResponse pResponseStatus_ =
  DescribeSeverityLevelsResponse'
    { severityLevels = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The available severity levels for the support case. Available severity levels are defined by your service level agreement with AWS.
--
-- /Note:/ Consider using 'severityLevels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrsSeverityLevels :: Lens.Lens' DescribeSeverityLevelsResponse (Lude.Maybe [SeverityLevel])
dslrsSeverityLevels = Lens.lens (severityLevels :: DescribeSeverityLevelsResponse -> Lude.Maybe [SeverityLevel]) (\s a -> s {severityLevels = a} :: DescribeSeverityLevelsResponse)
{-# DEPRECATED dslrsSeverityLevels "Use generic-lens or generic-optics with 'severityLevels' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrsResponseStatus :: Lens.Lens' DescribeSeverityLevelsResponse Lude.Int
dslrsResponseStatus = Lens.lens (responseStatus :: DescribeSeverityLevelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSeverityLevelsResponse)
{-# DEPRECATED dslrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
