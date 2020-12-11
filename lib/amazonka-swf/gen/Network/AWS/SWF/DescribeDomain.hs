{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified domain, including description and status.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.DescribeDomain
  ( -- * Creating a request
    DescribeDomain (..),
    mkDescribeDomain,

    -- ** Request lenses
    ddName,

    -- * Destructuring the response
    DescribeDomainResponse (..),
    mkDescribeDomainResponse,

    -- ** Response lenses
    ddrsResponseStatus,
    ddrsDomainInfo,
    ddrsConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkDescribeDomain' smart constructor.
newtype DescribeDomain = DescribeDomain' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomain' with the minimum fields required to make a request.
--
-- * 'name' - The name of the domain to describe.
mkDescribeDomain ::
  -- | 'name'
  Lude.Text ->
  DescribeDomain
mkDescribeDomain pName_ = DescribeDomain' {name = pName_}

-- | The name of the domain to describe.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DescribeDomain Lude.Text
ddName = Lens.lens (name :: DescribeDomain -> Lude.Text) (\s a -> s {name = a} :: DescribeDomain)
{-# DEPRECATED ddName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribeDomain where
  type Rs DescribeDomain = DescribeDomainResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDomainResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "domainInfo")
            Lude.<*> (x Lude..:> "configuration")
      )

instance Lude.ToHeaders DescribeDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.DescribeDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDomain where
  toJSON DescribeDomain' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])

instance Lude.ToPath DescribeDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDomain where
  toQuery = Lude.const Lude.mempty

-- | Contains details of a domain.
--
-- /See:/ 'mkDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { responseStatus ::
      Lude.Int,
    domainInfo :: DomainInfo,
    configuration :: DomainConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomainResponse' with the minimum fields required to make a request.
--
-- * 'configuration' - The domain configuration. Currently, this includes only the domain's retention period.
-- * 'domainInfo' - The basic information about a domain, such as its name, status, and description.
-- * 'responseStatus' - The response status code.
mkDescribeDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'domainInfo'
  DomainInfo ->
  -- | 'configuration'
  DomainConfiguration ->
  DescribeDomainResponse
mkDescribeDomainResponse
  pResponseStatus_
  pDomainInfo_
  pConfiguration_ =
    DescribeDomainResponse'
      { responseStatus = pResponseStatus_,
        domainInfo = pDomainInfo_,
        configuration = pConfiguration_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DescribeDomainResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DescribeDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The basic information about a domain, such as its name, status, and description.
--
-- /Note:/ Consider using 'domainInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDomainInfo :: Lens.Lens' DescribeDomainResponse DomainInfo
ddrsDomainInfo = Lens.lens (domainInfo :: DescribeDomainResponse -> DomainInfo) (\s a -> s {domainInfo = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsDomainInfo "Use generic-lens or generic-optics with 'domainInfo' instead." #-}

-- | The domain configuration. Currently, this includes only the domain's retention period.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsConfiguration :: Lens.Lens' DescribeDomainResponse DomainConfiguration
ddrsConfiguration = Lens.lens (configuration :: DescribeDomainResponse -> DomainConfiguration) (\s a -> s {configuration = a} :: DescribeDomainResponse)
{-# DEPRECATED ddrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}
