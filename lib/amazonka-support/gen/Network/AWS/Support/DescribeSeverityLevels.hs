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
    dslrrsSeverityLevels,
    dslrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkDescribeSeverityLevels' smart constructor.
newtype DescribeSeverityLevels = DescribeSeverityLevels'
  { -- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
    language :: Core.Maybe Types.Language
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSeverityLevels' value with any optional fields omitted.
mkDescribeSeverityLevels ::
  DescribeSeverityLevels
mkDescribeSeverityLevels =
  DescribeSeverityLevels' {language = Core.Nothing}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslLanguage :: Lens.Lens' DescribeSeverityLevels (Core.Maybe Types.Language)
dslLanguage = Lens.field @"language"
{-# DEPRECATED dslLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Core.FromJSON DescribeSeverityLevels where
  toJSON DescribeSeverityLevels {..} =
    Core.object
      (Core.catMaybes [("language" Core..=) Core.<$> language])

instance Core.AWSRequest DescribeSeverityLevels where
  type Rs DescribeSeverityLevels = DescribeSeverityLevelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSSupport_20130415.DescribeSeverityLevels")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSeverityLevelsResponse'
            Core.<$> (x Core..:? "severityLevels")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The list of severity levels returned by the 'DescribeSeverityLevels' operation.
--
-- /See:/ 'mkDescribeSeverityLevelsResponse' smart constructor.
data DescribeSeverityLevelsResponse = DescribeSeverityLevelsResponse'
  { -- | The available severity levels for the support case. Available severity levels are defined by your service level agreement with AWS.
    severityLevels :: Core.Maybe [Types.SeverityLevel],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSeverityLevelsResponse' value with any optional fields omitted.
mkDescribeSeverityLevelsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSeverityLevelsResponse
mkDescribeSeverityLevelsResponse responseStatus =
  DescribeSeverityLevelsResponse'
    { severityLevels = Core.Nothing,
      responseStatus
    }

-- | The available severity levels for the support case. Available severity levels are defined by your service level agreement with AWS.
--
-- /Note:/ Consider using 'severityLevels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrrsSeverityLevels :: Lens.Lens' DescribeSeverityLevelsResponse (Core.Maybe [Types.SeverityLevel])
dslrrsSeverityLevels = Lens.field @"severityLevels"
{-# DEPRECATED dslrrsSeverityLevels "Use generic-lens or generic-optics with 'severityLevels' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrrsResponseStatus :: Lens.Lens' DescribeSeverityLevelsResponse Core.Int
dslrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dslrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
