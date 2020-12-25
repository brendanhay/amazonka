{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorChecks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all available AWS Trusted Advisor checks, including the name, ID, category, description, and metadata. You must specify a language code. The AWS Support API currently supports English ("en") and Japanese ("ja"). The response contains a 'TrustedAdvisorCheckDescription' object for each check. You must set the AWS Region to us-east-1.
module Network.AWS.Support.DescribeTrustedAdvisorChecks
  ( -- * Creating a request
    DescribeTrustedAdvisorChecks (..),
    mkDescribeTrustedAdvisorChecks,

    -- ** Request lenses
    dtacLanguage,

    -- * Destructuring the response
    DescribeTrustedAdvisorChecksResponse (..),
    mkDescribeTrustedAdvisorChecksResponse,

    -- ** Response lenses
    dtacrrsChecks,
    dtacrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkDescribeTrustedAdvisorChecks' smart constructor.
newtype DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecks'
  { -- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
    language :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrustedAdvisorChecks' value with any optional fields omitted.
mkDescribeTrustedAdvisorChecks ::
  -- | 'language'
  Types.String ->
  DescribeTrustedAdvisorChecks
mkDescribeTrustedAdvisorChecks language =
  DescribeTrustedAdvisorChecks' {language}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacLanguage :: Lens.Lens' DescribeTrustedAdvisorChecks Types.String
dtacLanguage = Lens.field @"language"
{-# DEPRECATED dtacLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Core.FromJSON DescribeTrustedAdvisorChecks where
  toJSON DescribeTrustedAdvisorChecks {..} =
    Core.object
      (Core.catMaybes [Core.Just ("language" Core..= language)])

instance Core.AWSRequest DescribeTrustedAdvisorChecks where
  type
    Rs DescribeTrustedAdvisorChecks =
      DescribeTrustedAdvisorChecksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSSupport_20130415.DescribeTrustedAdvisorChecks"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorChecksResponse'
            Core.<$> (x Core..:? "checks" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Information about the Trusted Advisor checks returned by the 'DescribeTrustedAdvisorChecks' operation.
--
-- /See:/ 'mkDescribeTrustedAdvisorChecksResponse' smart constructor.
data DescribeTrustedAdvisorChecksResponse = DescribeTrustedAdvisorChecksResponse'
  { -- | Information about all available Trusted Advisor checks.
    checks :: [Types.TrustedAdvisorCheckDescription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrustedAdvisorChecksResponse' value with any optional fields omitted.
mkDescribeTrustedAdvisorChecksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTrustedAdvisorChecksResponse
mkDescribeTrustedAdvisorChecksResponse responseStatus =
  DescribeTrustedAdvisorChecksResponse'
    { checks = Core.mempty,
      responseStatus
    }

-- | Information about all available Trusted Advisor checks.
--
-- /Note:/ Consider using 'checks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrrsChecks :: Lens.Lens' DescribeTrustedAdvisorChecksResponse [Types.TrustedAdvisorCheckDescription]
dtacrrsChecks = Lens.field @"checks"
{-# DEPRECATED dtacrrsChecks "Use generic-lens or generic-optics with 'checks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrrsResponseStatus :: Lens.Lens' DescribeTrustedAdvisorChecksResponse Core.Int
dtacrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
