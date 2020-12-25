{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.BuildSuggesters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indexes the search suggestions. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html#configuring-suggesters Configuring Suggesters> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.BuildSuggesters
  ( -- * Creating a request
    BuildSuggesters (..),
    mkBuildSuggesters,

    -- ** Request lenses
    bsDomainName,

    -- * Destructuring the response
    BuildSuggestersResponse (..),
    mkBuildSuggestersResponse,

    -- ** Response lenses
    bsrrsFieldNames,
    bsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'BuildSuggester' @ operation. Specifies the name of the domain you want to update.
--
-- /See:/ 'mkBuildSuggesters' smart constructor.
newtype BuildSuggesters = BuildSuggesters'
  { domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BuildSuggesters' value with any optional fields omitted.
mkBuildSuggesters ::
  -- | 'domainName'
  Types.DomainName ->
  BuildSuggesters
mkBuildSuggesters domainName = BuildSuggesters' {domainName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsDomainName :: Lens.Lens' BuildSuggesters Types.DomainName
bsDomainName = Lens.field @"domainName"
{-# DEPRECATED bsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.AWSRequest BuildSuggesters where
  type Rs BuildSuggesters = BuildSuggestersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "BuildSuggesters")
                Core.<> (Core.pure ("Version", "2013-01-01"))
                Core.<> (Core.toQueryValue "DomainName" domainName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "BuildSuggestersResult"
      ( \s h x ->
          BuildSuggestersResponse'
            Core.<$> (x Core..@? "FieldNames" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of a @BuildSuggester@ request. Contains a list of the fields used for suggestions.
--
-- /See:/ 'mkBuildSuggestersResponse' smart constructor.
data BuildSuggestersResponse = BuildSuggestersResponse'
  { fieldNames :: Core.Maybe [Types.FieldName],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BuildSuggestersResponse' value with any optional fields omitted.
mkBuildSuggestersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BuildSuggestersResponse
mkBuildSuggestersResponse responseStatus =
  BuildSuggestersResponse'
    { fieldNames = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'fieldNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrrsFieldNames :: Lens.Lens' BuildSuggestersResponse (Core.Maybe [Types.FieldName])
bsrrsFieldNames = Lens.field @"fieldNames"
{-# DEPRECATED bsrrsFieldNames "Use generic-lens or generic-optics with 'fieldNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrrsResponseStatus :: Lens.Lens' BuildSuggestersResponse Core.Int
bsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
