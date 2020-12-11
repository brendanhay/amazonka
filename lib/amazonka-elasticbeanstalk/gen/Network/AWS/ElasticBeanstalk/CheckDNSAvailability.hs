{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CheckDNSAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks if the specified CNAME is available.
module Network.AWS.ElasticBeanstalk.CheckDNSAvailability
  ( -- * Creating a request
    CheckDNSAvailability (..),
    mkCheckDNSAvailability,

    -- ** Request lenses
    cdaCNAMEPrefix,

    -- * Destructuring the response
    CheckDNSAvailabilityResponse (..),
    mkCheckDNSAvailabilityResponse,

    -- ** Response lenses
    cdarsFullyQualifiedCNAME,
    cdarsAvailable,
    cdarsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Results message indicating whether a CNAME is available.
--
-- /See:/ 'mkCheckDNSAvailability' smart constructor.
newtype CheckDNSAvailability = CheckDNSAvailability'
  { cNAMEPrefix ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckDNSAvailability' with the minimum fields required to make a request.
--
-- * 'cNAMEPrefix' - The prefix used when this CNAME is reserved.
mkCheckDNSAvailability ::
  -- | 'cNAMEPrefix'
  Lude.Text ->
  CheckDNSAvailability
mkCheckDNSAvailability pCNAMEPrefix_ =
  CheckDNSAvailability' {cNAMEPrefix = pCNAMEPrefix_}

-- | The prefix used when this CNAME is reserved.
--
-- /Note:/ Consider using 'cNAMEPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaCNAMEPrefix :: Lens.Lens' CheckDNSAvailability Lude.Text
cdaCNAMEPrefix = Lens.lens (cNAMEPrefix :: CheckDNSAvailability -> Lude.Text) (\s a -> s {cNAMEPrefix = a} :: CheckDNSAvailability)
{-# DEPRECATED cdaCNAMEPrefix "Use generic-lens or generic-optics with 'cNAMEPrefix' instead." #-}

instance Lude.AWSRequest CheckDNSAvailability where
  type Rs CheckDNSAvailability = CheckDNSAvailabilityResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "CheckDNSAvailabilityResult"
      ( \s h x ->
          CheckDNSAvailabilityResponse'
            Lude.<$> (x Lude..@? "FullyQualifiedCNAME")
            Lude.<*> (x Lude..@? "Available")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CheckDNSAvailability where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CheckDNSAvailability where
  toPath = Lude.const "/"

instance Lude.ToQuery CheckDNSAvailability where
  toQuery CheckDNSAvailability' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CheckDNSAvailability" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "CNAMEPrefix" Lude.=: cNAMEPrefix
      ]

-- | Indicates if the specified CNAME is available.
--
-- /See:/ 'mkCheckDNSAvailabilityResponse' smart constructor.
data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse'
  { fullyQualifiedCNAME ::
      Lude.Maybe Lude.Text,
    available :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'CheckDNSAvailabilityResponse' with the minimum fields required to make a request.
--
-- * 'available' - Indicates if the specified CNAME is available:
--
--
--     * @true@ : The CNAME is available.
--
--
--     * @false@ : The CNAME is not available.
--
--
-- * 'fullyQualifiedCNAME' - The fully qualified CNAME to reserve when 'CreateEnvironment' is called with the provided prefix.
-- * 'responseStatus' - The response status code.
mkCheckDNSAvailabilityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CheckDNSAvailabilityResponse
mkCheckDNSAvailabilityResponse pResponseStatus_ =
  CheckDNSAvailabilityResponse'
    { fullyQualifiedCNAME = Lude.Nothing,
      available = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The fully qualified CNAME to reserve when 'CreateEnvironment' is called with the provided prefix.
--
-- /Note:/ Consider using 'fullyQualifiedCNAME' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdarsFullyQualifiedCNAME :: Lens.Lens' CheckDNSAvailabilityResponse (Lude.Maybe Lude.Text)
cdarsFullyQualifiedCNAME = Lens.lens (fullyQualifiedCNAME :: CheckDNSAvailabilityResponse -> Lude.Maybe Lude.Text) (\s a -> s {fullyQualifiedCNAME = a} :: CheckDNSAvailabilityResponse)
{-# DEPRECATED cdarsFullyQualifiedCNAME "Use generic-lens or generic-optics with 'fullyQualifiedCNAME' instead." #-}

-- | Indicates if the specified CNAME is available:
--
--
--     * @true@ : The CNAME is available.
--
--
--     * @false@ : The CNAME is not available.
--
--
--
-- /Note:/ Consider using 'available' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdarsAvailable :: Lens.Lens' CheckDNSAvailabilityResponse (Lude.Maybe Lude.Bool)
cdarsAvailable = Lens.lens (available :: CheckDNSAvailabilityResponse -> Lude.Maybe Lude.Bool) (\s a -> s {available = a} :: CheckDNSAvailabilityResponse)
{-# DEPRECATED cdarsAvailable "Use generic-lens or generic-optics with 'available' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdarsResponseStatus :: Lens.Lens' CheckDNSAvailabilityResponse Lude.Int
cdarsResponseStatus = Lens.lens (responseStatus :: CheckDNSAvailabilityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CheckDNSAvailabilityResponse)
{-# DEPRECATED cdarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
