{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a previously associated principal ARN from a specified portfolio.
module Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
  ( -- * Creating a request
    DisassociatePrincipalFromPortfolio (..),
    mkDisassociatePrincipalFromPortfolio,

    -- ** Request lenses
    disAcceptLanguage,
    disPortfolioId,
    disPrincipalARN,

    -- * Destructuring the response
    DisassociatePrincipalFromPortfolioResponse (..),
    mkDisassociatePrincipalFromPortfolioResponse,

    -- ** Response lenses
    dpfprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDisassociatePrincipalFromPortfolio' smart constructor.
data DisassociatePrincipalFromPortfolio = DisassociatePrincipalFromPortfolio'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    portfolioId ::
      Lude.Text,
    principalARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociatePrincipalFromPortfolio' with the minimum fields required to make a request.
--
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'portfolioId' - The portfolio identifier.
-- * 'principalARN' - The ARN of the principal (IAM user, role, or group).
mkDisassociatePrincipalFromPortfolio ::
  -- | 'portfolioId'
  Lude.Text ->
  -- | 'principalARN'
  Lude.Text ->
  DisassociatePrincipalFromPortfolio
mkDisassociatePrincipalFromPortfolio pPortfolioId_ pPrincipalARN_ =
  DisassociatePrincipalFromPortfolio'
    { acceptLanguage =
        Lude.Nothing,
      portfolioId = pPortfolioId_,
      principalARN = pPrincipalARN_
    }

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disAcceptLanguage :: Lens.Lens' DisassociatePrincipalFromPortfolio (Lude.Maybe Lude.Text)
disAcceptLanguage = Lens.lens (acceptLanguage :: DisassociatePrincipalFromPortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DisassociatePrincipalFromPortfolio)
{-# DEPRECATED disAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disPortfolioId :: Lens.Lens' DisassociatePrincipalFromPortfolio Lude.Text
disPortfolioId = Lens.lens (portfolioId :: DisassociatePrincipalFromPortfolio -> Lude.Text) (\s a -> s {portfolioId = a} :: DisassociatePrincipalFromPortfolio)
{-# DEPRECATED disPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The ARN of the principal (IAM user, role, or group).
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disPrincipalARN :: Lens.Lens' DisassociatePrincipalFromPortfolio Lude.Text
disPrincipalARN = Lens.lens (principalARN :: DisassociatePrincipalFromPortfolio -> Lude.Text) (\s a -> s {principalARN = a} :: DisassociatePrincipalFromPortfolio)
{-# DEPRECATED disPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

instance Lude.AWSRequest DisassociatePrincipalFromPortfolio where
  type
    Rs DisassociatePrincipalFromPortfolio =
      DisassociatePrincipalFromPortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociatePrincipalFromPortfolioResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociatePrincipalFromPortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DisassociatePrincipalFromPortfolio" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociatePrincipalFromPortfolio where
  toJSON DisassociatePrincipalFromPortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("PortfolioId" Lude..= portfolioId),
            Lude.Just ("PrincipalARN" Lude..= principalARN)
          ]
      )

instance Lude.ToPath DisassociatePrincipalFromPortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociatePrincipalFromPortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociatePrincipalFromPortfolioResponse' smart constructor.
newtype DisassociatePrincipalFromPortfolioResponse = DisassociatePrincipalFromPortfolioResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociatePrincipalFromPortfolioResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociatePrincipalFromPortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociatePrincipalFromPortfolioResponse
mkDisassociatePrincipalFromPortfolioResponse pResponseStatus_ =
  DisassociatePrincipalFromPortfolioResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfprsResponseStatus :: Lens.Lens' DisassociatePrincipalFromPortfolioResponse Lude.Int
dpfprsResponseStatus = Lens.lens (responseStatus :: DisassociatePrincipalFromPortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociatePrincipalFromPortfolioResponse)
{-# DEPRECATED dpfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
