{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeIndexFields
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the index fields configured for the search domain. Can be limited to specific fields by name. By default, shows all fields and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-domain-info.html Getting Domain Information> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeIndexFields
    (
    -- * Creating a request
      DescribeIndexFields (..)
    , mkDescribeIndexFields
    -- ** Request lenses
    , difDomainName
    , difDeployed
    , difFieldNames

    -- * Destructuring the response
    , DescribeIndexFieldsResponse (..)
    , mkDescribeIndexFieldsResponse
    -- ** Response lenses
    , difrgrsIndexFields
    , difrgrsResponseStatus
    ) where

import qualified Network.AWS.CloudSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @'DescribeIndexFields' @ operation. Specifies the name of the domain you want to describe. To restrict the response to particular index fields, specify the names of the index fields you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeIndexFields' smart constructor.
data DescribeIndexFields = DescribeIndexFields'
  { domainName :: Types.DomainName
    -- ^ The name of the domain you want to describe.
  , deployed :: Core.Maybe Core.Bool
    -- ^ Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
  , fieldNames :: Core.Maybe [Types.DynamicFieldName]
    -- ^ A list of the index fields you want to describe. If not specified, information is returned for all configured index fields.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIndexFields' value with any optional fields omitted.
mkDescribeIndexFields
    :: Types.DomainName -- ^ 'domainName'
    -> DescribeIndexFields
mkDescribeIndexFields domainName
  = DescribeIndexFields'{domainName, deployed = Core.Nothing,
                         fieldNames = Core.Nothing}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difDomainName :: Lens.Lens' DescribeIndexFields Types.DomainName
difDomainName = Lens.field @"domainName"
{-# INLINEABLE difDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difDeployed :: Lens.Lens' DescribeIndexFields (Core.Maybe Core.Bool)
difDeployed = Lens.field @"deployed"
{-# INLINEABLE difDeployed #-}
{-# DEPRECATED deployed "Use generic-lens or generic-optics with 'deployed' instead"  #-}

-- | A list of the index fields you want to describe. If not specified, information is returned for all configured index fields.
--
-- /Note:/ Consider using 'fieldNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difFieldNames :: Lens.Lens' DescribeIndexFields (Core.Maybe [Types.DynamicFieldName])
difFieldNames = Lens.field @"fieldNames"
{-# INLINEABLE difFieldNames #-}
{-# DEPRECATED fieldNames "Use generic-lens or generic-optics with 'fieldNames' instead"  #-}

instance Core.ToQuery DescribeIndexFields where
        toQuery DescribeIndexFields{..}
          = Core.toQueryPair "Action" ("DescribeIndexFields" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2013-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "DomainName" domainName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Deployed") deployed
              Core.<>
              Core.toQueryPair "FieldNames"
                (Core.maybe Core.mempty (Core.toQueryList "member") fieldNames)

instance Core.ToHeaders DescribeIndexFields where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeIndexFields where
        type Rs DescribeIndexFields = DescribeIndexFieldsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeIndexFieldsResult"
              (\ s h x ->
                 DescribeIndexFieldsResponse' Core.<$>
                   (x Core..@ "IndexFields" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a @DescribeIndexFields@ request. Contains the index fields configured for the domain specified in the request.
--
-- /See:/ 'mkDescribeIndexFieldsResponse' smart constructor.
data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse'
  { indexFields :: [Types.IndexFieldStatus]
    -- ^ The index fields configured for the domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeIndexFieldsResponse' value with any optional fields omitted.
mkDescribeIndexFieldsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeIndexFieldsResponse
mkDescribeIndexFieldsResponse responseStatus
  = DescribeIndexFieldsResponse'{indexFields = Core.mempty,
                                 responseStatus}

-- | The index fields configured for the domain.
--
-- /Note:/ Consider using 'indexFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrgrsIndexFields :: Lens.Lens' DescribeIndexFieldsResponse [Types.IndexFieldStatus]
difrgrsIndexFields = Lens.field @"indexFields"
{-# INLINEABLE difrgrsIndexFields #-}
{-# DEPRECATED indexFields "Use generic-lens or generic-optics with 'indexFields' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrgrsResponseStatus :: Lens.Lens' DescribeIndexFieldsResponse Core.Int
difrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE difrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
