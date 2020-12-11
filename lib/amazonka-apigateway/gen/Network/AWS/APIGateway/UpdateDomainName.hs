{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateDomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the 'DomainName' resource.
module Network.AWS.APIGateway.UpdateDomainName
  ( -- * Creating a request
    UpdateDomainName (..),
    mkUpdateDomainName,

    -- ** Request lenses
    udnPatchOperations,
    udnDomainName,

    -- * Destructuring the response
    DomainName (..),
    mkDomainName,

    -- ** Response lenses
    dnRegionalHostedZoneId,
    dnCertificateName,
    dnRegionalCertificateARN,
    dnCertificateARN,
    dnDistributionHostedZoneId,
    dnSecurityPolicy,
    dnDomainName,
    dnMutualTLSAuthentication,
    dnRegionalCertificateName,
    dnRegionalDomainName,
    dnCertificateUploadDate,
    dnDistributionDomainName,
    dnDomainNameStatusMessage,
    dnEndpointConfiguration,
    dnDomainNameStatus,
    dnTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to change information about the 'DomainName' resource.
--
-- /See:/ 'mkUpdateDomainName' smart constructor.
data UpdateDomainName = UpdateDomainName'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    domainName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainName' with the minimum fields required to make a request.
--
-- * 'domainName' - [Required] The name of the 'DomainName' resource to be changed.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateDomainName ::
  -- | 'domainName'
  Lude.Text ->
  UpdateDomainName
mkUpdateDomainName pDomainName_ =
  UpdateDomainName'
    { patchOperations = Lude.Nothing,
      domainName = pDomainName_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnPatchOperations :: Lens.Lens' UpdateDomainName (Lude.Maybe [PatchOperation])
udnPatchOperations = Lens.lens (patchOperations :: UpdateDomainName -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateDomainName)
{-# DEPRECATED udnPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The name of the 'DomainName' resource to be changed.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnDomainName :: Lens.Lens' UpdateDomainName Lude.Text
udnDomainName = Lens.lens (domainName :: UpdateDomainName -> Lude.Text) (\s a -> s {domainName = a} :: UpdateDomainName)
{-# DEPRECATED udnDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest UpdateDomainName where
  type Rs UpdateDomainName = DomainName
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateDomainName where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateDomainName where
  toJSON UpdateDomainName' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateDomainName where
  toPath UpdateDomainName' {..} =
    Lude.mconcat ["/domainnames/", Lude.toBS domainName]

instance Lude.ToQuery UpdateDomainName where
  toQuery = Lude.const Lude.mempty
