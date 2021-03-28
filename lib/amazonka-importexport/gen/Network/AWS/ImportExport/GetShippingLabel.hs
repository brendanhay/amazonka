{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.GetShippingLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation generates a pre-paid UPS shipping label that you will use to ship your device to AWS for processing.
module Network.AWS.ImportExport.GetShippingLabel
    (
    -- * Creating a request
      GetShippingLabel (..)
    , mkGetShippingLabel
    -- ** Request lenses
    , gslJobIds
    , gslAPIVersion
    , gslCity
    , gslCompany
    , gslCountry
    , gslName
    , gslPhoneNumber
    , gslPostalCode
    , gslStateOrProvince
    , gslStreet1
    , gslStreet2
    , gslStreet3

    -- * Destructuring the response
    , GetShippingLabelResponse (..)
    , mkGetShippingLabelResponse
    -- ** Response lenses
    , gslrrsShippingLabelURL
    , gslrrsWarning
    , gslrrsResponseStatus
    ) where

import qualified Network.AWS.ImportExport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetShippingLabel' smart constructor.
data GetShippingLabel = GetShippingLabel'
  { jobIds :: [Types.GenericString]
  , aPIVersion :: Core.Maybe Types.APIVersion
  , city :: Core.Maybe Types.City
  , company :: Core.Maybe Types.Company
  , country :: Core.Maybe Types.Country
  , name :: Core.Maybe Types.Name
  , phoneNumber :: Core.Maybe Types.PhoneNumber
  , postalCode :: Core.Maybe Types.PostalCode
  , stateOrProvince :: Core.Maybe Types.StateOrProvince
  , street1 :: Core.Maybe Types.Street1
  , street2 :: Core.Maybe Types.Street2
  , street3 :: Core.Maybe Types.Street3
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetShippingLabel' value with any optional fields omitted.
mkGetShippingLabel
    :: GetShippingLabel
mkGetShippingLabel
  = GetShippingLabel'{jobIds = Core.mempty,
                      aPIVersion = Core.Nothing, city = Core.Nothing,
                      company = Core.Nothing, country = Core.Nothing,
                      name = Core.Nothing, phoneNumber = Core.Nothing,
                      postalCode = Core.Nothing, stateOrProvince = Core.Nothing,
                      street1 = Core.Nothing, street2 = Core.Nothing,
                      street3 = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslJobIds :: Lens.Lens' GetShippingLabel [Types.GenericString]
gslJobIds = Lens.field @"jobIds"
{-# INLINEABLE gslJobIds #-}
{-# DEPRECATED jobIds "Use generic-lens or generic-optics with 'jobIds' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPIVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslAPIVersion :: Lens.Lens' GetShippingLabel (Core.Maybe Types.APIVersion)
gslAPIVersion = Lens.field @"aPIVersion"
{-# INLINEABLE gslAPIVersion #-}
{-# DEPRECATED aPIVersion "Use generic-lens or generic-optics with 'aPIVersion' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslCity :: Lens.Lens' GetShippingLabel (Core.Maybe Types.City)
gslCity = Lens.field @"city"
{-# INLINEABLE gslCity #-}
{-# DEPRECATED city "Use generic-lens or generic-optics with 'city' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'company' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslCompany :: Lens.Lens' GetShippingLabel (Core.Maybe Types.Company)
gslCompany = Lens.field @"company"
{-# INLINEABLE gslCompany #-}
{-# DEPRECATED company "Use generic-lens or generic-optics with 'company' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslCountry :: Lens.Lens' GetShippingLabel (Core.Maybe Types.Country)
gslCountry = Lens.field @"country"
{-# INLINEABLE gslCountry #-}
{-# DEPRECATED country "Use generic-lens or generic-optics with 'country' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslName :: Lens.Lens' GetShippingLabel (Core.Maybe Types.Name)
gslName = Lens.field @"name"
{-# INLINEABLE gslName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslPhoneNumber :: Lens.Lens' GetShippingLabel (Core.Maybe Types.PhoneNumber)
gslPhoneNumber = Lens.field @"phoneNumber"
{-# INLINEABLE gslPhoneNumber #-}
{-# DEPRECATED phoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'postalCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslPostalCode :: Lens.Lens' GetShippingLabel (Core.Maybe Types.PostalCode)
gslPostalCode = Lens.field @"postalCode"
{-# INLINEABLE gslPostalCode #-}
{-# DEPRECATED postalCode "Use generic-lens or generic-optics with 'postalCode' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stateOrProvince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslStateOrProvince :: Lens.Lens' GetShippingLabel (Core.Maybe Types.StateOrProvince)
gslStateOrProvince = Lens.field @"stateOrProvince"
{-# INLINEABLE gslStateOrProvince #-}
{-# DEPRECATED stateOrProvince "Use generic-lens or generic-optics with 'stateOrProvince' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'street1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslStreet1 :: Lens.Lens' GetShippingLabel (Core.Maybe Types.Street1)
gslStreet1 = Lens.field @"street1"
{-# INLINEABLE gslStreet1 #-}
{-# DEPRECATED street1 "Use generic-lens or generic-optics with 'street1' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'street2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslStreet2 :: Lens.Lens' GetShippingLabel (Core.Maybe Types.Street2)
gslStreet2 = Lens.field @"street2"
{-# INLINEABLE gslStreet2 #-}
{-# DEPRECATED street2 "Use generic-lens or generic-optics with 'street2' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'street3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslStreet3 :: Lens.Lens' GetShippingLabel (Core.Maybe Types.Street3)
gslStreet3 = Lens.field @"street3"
{-# INLINEABLE gslStreet3 #-}
{-# DEPRECATED street3 "Use generic-lens or generic-optics with 'street3' instead"  #-}

instance Core.ToQuery GetShippingLabel where
        toQuery GetShippingLabel{..}
          = Core.toQueryPair "Operation=GetShippingLabel" ("" :: Core.Text)
              Core.<> Core.toQueryPair "Action" ("GetShippingLabel" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-06-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "jobIds" (Core.toQueryList "member" jobIds)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "APIVersion") aPIVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "city") city
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "company") company
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "country") country
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "name") name
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "phoneNumber") phoneNumber
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "postalCode") postalCode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "stateOrProvince")
                stateOrProvince
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "street1") street1
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "street2") street2
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "street3") street3

instance Core.ToHeaders GetShippingLabel where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetShippingLabel where
        type Rs GetShippingLabel = GetShippingLabelResponse
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
          = Response.receiveXMLWrapper "GetShippingLabelResult"
              (\ s h x ->
                 GetShippingLabelResponse' Core.<$>
                   (x Core..@? "ShippingLabelURL") Core.<*> x Core..@? "Warning"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetShippingLabelResponse' smart constructor.
data GetShippingLabelResponse = GetShippingLabelResponse'
  { shippingLabelURL :: Core.Maybe Types.GenericString
  , warning :: Core.Maybe Types.GenericString
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetShippingLabelResponse' value with any optional fields omitted.
mkGetShippingLabelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetShippingLabelResponse
mkGetShippingLabelResponse responseStatus
  = GetShippingLabelResponse'{shippingLabelURL = Core.Nothing,
                              warning = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'shippingLabelURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrrsShippingLabelURL :: Lens.Lens' GetShippingLabelResponse (Core.Maybe Types.GenericString)
gslrrsShippingLabelURL = Lens.field @"shippingLabelURL"
{-# INLINEABLE gslrrsShippingLabelURL #-}
{-# DEPRECATED shippingLabelURL "Use generic-lens or generic-optics with 'shippingLabelURL' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrrsWarning :: Lens.Lens' GetShippingLabelResponse (Core.Maybe Types.GenericString)
gslrrsWarning = Lens.field @"warning"
{-# INLINEABLE gslrrsWarning #-}
{-# DEPRECATED warning "Use generic-lens or generic-optics with 'warning' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrrsResponseStatus :: Lens.Lens' GetShippingLabelResponse Core.Int
gslrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gslrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
