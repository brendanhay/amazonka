{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetShippingLabel (..),
    mkGetShippingLabel,

    -- ** Request lenses
    gslStreet3,
    gslAPIVersion,
    gslCountry,
    gslStateOrProvince,
    gslPostalCode,
    gslStreet2,
    gslName,
    gslCompany,
    gslPhoneNumber,
    gslCity,
    gslStreet1,
    gslJobIds,

    -- * Destructuring the response
    GetShippingLabelResponse (..),
    mkGetShippingLabelResponse,

    -- ** Response lenses
    gslrsShippingLabelURL,
    gslrsWarning,
    gslrsResponseStatus,
  )
where

import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetShippingLabel' smart constructor.
data GetShippingLabel = GetShippingLabel'
  { street3 :: Lude.Maybe Lude.Text,
    apiVersion :: Lude.Maybe Lude.Text,
    country :: Lude.Maybe Lude.Text,
    stateOrProvince :: Lude.Maybe Lude.Text,
    postalCode :: Lude.Maybe Lude.Text,
    street2 :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    company :: Lude.Maybe Lude.Text,
    phoneNumber :: Lude.Maybe Lude.Text,
    city :: Lude.Maybe Lude.Text,
    street1 :: Lude.Maybe Lude.Text,
    jobIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetShippingLabel' with the minimum fields required to make a request.
--
-- * 'street3' -
-- * 'apiVersion' -
-- * 'country' -
-- * 'stateOrProvince' -
-- * 'postalCode' -
-- * 'street2' -
-- * 'name' -
-- * 'company' -
-- * 'phoneNumber' -
-- * 'city' -
-- * 'street1' -
-- * 'jobIds' -
mkGetShippingLabel ::
  GetShippingLabel
mkGetShippingLabel =
  GetShippingLabel'
    { street3 = Lude.Nothing,
      apiVersion = Lude.Nothing,
      country = Lude.Nothing,
      stateOrProvince = Lude.Nothing,
      postalCode = Lude.Nothing,
      street2 = Lude.Nothing,
      name = Lude.Nothing,
      company = Lude.Nothing,
      phoneNumber = Lude.Nothing,
      city = Lude.Nothing,
      street1 = Lude.Nothing,
      jobIds = Lude.mempty
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'street3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslStreet3 :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslStreet3 = Lens.lens (street3 :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {street3 = a} :: GetShippingLabel)
{-# DEPRECATED gslStreet3 "Use generic-lens or generic-optics with 'street3' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslAPIVersion :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslAPIVersion = Lens.lens (apiVersion :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {apiVersion = a} :: GetShippingLabel)
{-# DEPRECATED gslAPIVersion "Use generic-lens or generic-optics with 'apiVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslCountry :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslCountry = Lens.lens (country :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {country = a} :: GetShippingLabel)
{-# DEPRECATED gslCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stateOrProvince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslStateOrProvince :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslStateOrProvince = Lens.lens (stateOrProvince :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {stateOrProvince = a} :: GetShippingLabel)
{-# DEPRECATED gslStateOrProvince "Use generic-lens or generic-optics with 'stateOrProvince' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'postalCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslPostalCode :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslPostalCode = Lens.lens (postalCode :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {postalCode = a} :: GetShippingLabel)
{-# DEPRECATED gslPostalCode "Use generic-lens or generic-optics with 'postalCode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'street2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslStreet2 :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslStreet2 = Lens.lens (street2 :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {street2 = a} :: GetShippingLabel)
{-# DEPRECATED gslStreet2 "Use generic-lens or generic-optics with 'street2' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslName :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslName = Lens.lens (name :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetShippingLabel)
{-# DEPRECATED gslName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'company' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslCompany :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslCompany = Lens.lens (company :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {company = a} :: GetShippingLabel)
{-# DEPRECATED gslCompany "Use generic-lens or generic-optics with 'company' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslPhoneNumber :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslPhoneNumber = Lens.lens (phoneNumber :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {phoneNumber = a} :: GetShippingLabel)
{-# DEPRECATED gslPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslCity :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslCity = Lens.lens (city :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {city = a} :: GetShippingLabel)
{-# DEPRECATED gslCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'street1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslStreet1 :: Lens.Lens' GetShippingLabel (Lude.Maybe Lude.Text)
gslStreet1 = Lens.lens (street1 :: GetShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {street1 = a} :: GetShippingLabel)
{-# DEPRECATED gslStreet1 "Use generic-lens or generic-optics with 'street1' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslJobIds :: Lens.Lens' GetShippingLabel [Lude.Text]
gslJobIds = Lens.lens (jobIds :: GetShippingLabel -> [Lude.Text]) (\s a -> s {jobIds = a} :: GetShippingLabel)
{-# DEPRECATED gslJobIds "Use generic-lens or generic-optics with 'jobIds' instead." #-}

instance Lude.AWSRequest GetShippingLabel where
  type Rs GetShippingLabel = GetShippingLabelResponse
  request = Req.postQuery importExportService
  response =
    Res.receiveXMLWrapper
      "GetShippingLabelResult"
      ( \s h x ->
          GetShippingLabelResponse'
            Lude.<$> (x Lude..@? "ShippingLabelURL")
            Lude.<*> (x Lude..@? "Warning")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetShippingLabel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetShippingLabel where
  toPath = Lude.const "/"

instance Lude.ToQuery GetShippingLabel where
  toQuery GetShippingLabel' {..} =
    Lude.mconcat
      [ "Operation=GetShippingLabel",
        "Action" Lude.=: ("GetShippingLabel" :: Lude.ByteString),
        "Version" Lude.=: ("2010-06-01" :: Lude.ByteString),
        "street3" Lude.=: street3,
        "APIVersion" Lude.=: apiVersion,
        "country" Lude.=: country,
        "stateOrProvince" Lude.=: stateOrProvince,
        "postalCode" Lude.=: postalCode,
        "street2" Lude.=: street2,
        "name" Lude.=: name,
        "company" Lude.=: company,
        "phoneNumber" Lude.=: phoneNumber,
        "city" Lude.=: city,
        "street1" Lude.=: street1,
        "jobIds" Lude.=: Lude.toQueryList "member" jobIds
      ]

-- | /See:/ 'mkGetShippingLabelResponse' smart constructor.
data GetShippingLabelResponse = GetShippingLabelResponse'
  { shippingLabelURL :: Lude.Maybe Lude.Text,
    warning :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetShippingLabelResponse' with the minimum fields required to make a request.
--
-- * 'shippingLabelURL' -
-- * 'warning' -
-- * 'responseStatus' - The response status code.
mkGetShippingLabelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetShippingLabelResponse
mkGetShippingLabelResponse pResponseStatus_ =
  GetShippingLabelResponse'
    { shippingLabelURL = Lude.Nothing,
      warning = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'shippingLabelURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrsShippingLabelURL :: Lens.Lens' GetShippingLabelResponse (Lude.Maybe Lude.Text)
gslrsShippingLabelURL = Lens.lens (shippingLabelURL :: GetShippingLabelResponse -> Lude.Maybe Lude.Text) (\s a -> s {shippingLabelURL = a} :: GetShippingLabelResponse)
{-# DEPRECATED gslrsShippingLabelURL "Use generic-lens or generic-optics with 'shippingLabelURL' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrsWarning :: Lens.Lens' GetShippingLabelResponse (Lude.Maybe Lude.Text)
gslrsWarning = Lens.lens (warning :: GetShippingLabelResponse -> Lude.Maybe Lude.Text) (\s a -> s {warning = a} :: GetShippingLabelResponse)
{-# DEPRECATED gslrsWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrsResponseStatus :: Lens.Lens' GetShippingLabelResponse Lude.Int
gslrsResponseStatus = Lens.lens (responseStatus :: GetShippingLabelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetShippingLabelResponse)
{-# DEPRECATED gslrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
