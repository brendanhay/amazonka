{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified principal ARN with the specified portfolio.
module Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
  ( -- * Creating a request
    AssociatePrincipalWithPortfolio (..),
    mkAssociatePrincipalWithPortfolio,

    -- ** Request lenses
    aAcceptLanguage,
    aPortfolioId,
    aPrincipalARN,
    aPrincipalType,

    -- * Destructuring the response
    AssociatePrincipalWithPortfolioResponse (..),
    mkAssociatePrincipalWithPortfolioResponse,

    -- ** Response lenses
    apwprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkAssociatePrincipalWithPortfolio' smart constructor.
data AssociatePrincipalWithPortfolio = AssociatePrincipalWithPortfolio'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    portfolioId :: Lude.Text,
    principalARN :: Lude.Text,
    principalType ::
      PrincipalType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociatePrincipalWithPortfolio' with the minimum fields required to make a request.
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
-- * 'principalType' - The principal type. The supported value is @IAM@ .
mkAssociatePrincipalWithPortfolio ::
  -- | 'portfolioId'
  Lude.Text ->
  -- | 'principalARN'
  Lude.Text ->
  -- | 'principalType'
  PrincipalType ->
  AssociatePrincipalWithPortfolio
mkAssociatePrincipalWithPortfolio
  pPortfolioId_
  pPrincipalARN_
  pPrincipalType_ =
    AssociatePrincipalWithPortfolio'
      { acceptLanguage = Lude.Nothing,
        portfolioId = pPortfolioId_,
        principalARN = pPrincipalARN_,
        principalType = pPrincipalType_
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
aAcceptLanguage :: Lens.Lens' AssociatePrincipalWithPortfolio (Lude.Maybe Lude.Text)
aAcceptLanguage = Lens.lens (acceptLanguage :: AssociatePrincipalWithPortfolio -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: AssociatePrincipalWithPortfolio)
{-# DEPRECATED aAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPortfolioId :: Lens.Lens' AssociatePrincipalWithPortfolio Lude.Text
aPortfolioId = Lens.lens (portfolioId :: AssociatePrincipalWithPortfolio -> Lude.Text) (\s a -> s {portfolioId = a} :: AssociatePrincipalWithPortfolio)
{-# DEPRECATED aPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The ARN of the principal (IAM user, role, or group).
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrincipalARN :: Lens.Lens' AssociatePrincipalWithPortfolio Lude.Text
aPrincipalARN = Lens.lens (principalARN :: AssociatePrincipalWithPortfolio -> Lude.Text) (\s a -> s {principalARN = a} :: AssociatePrincipalWithPortfolio)
{-# DEPRECATED aPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | The principal type. The supported value is @IAM@ .
--
-- /Note:/ Consider using 'principalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrincipalType :: Lens.Lens' AssociatePrincipalWithPortfolio PrincipalType
aPrincipalType = Lens.lens (principalType :: AssociatePrincipalWithPortfolio -> PrincipalType) (\s a -> s {principalType = a} :: AssociatePrincipalWithPortfolio)
{-# DEPRECATED aPrincipalType "Use generic-lens or generic-optics with 'principalType' instead." #-}

instance Lude.AWSRequest AssociatePrincipalWithPortfolio where
  type
    Rs AssociatePrincipalWithPortfolio =
      AssociatePrincipalWithPortfolioResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociatePrincipalWithPortfolioResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociatePrincipalWithPortfolio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.AssociatePrincipalWithPortfolio" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociatePrincipalWithPortfolio where
  toJSON AssociatePrincipalWithPortfolio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("PortfolioId" Lude..= portfolioId),
            Lude.Just ("PrincipalARN" Lude..= principalARN),
            Lude.Just ("PrincipalType" Lude..= principalType)
          ]
      )

instance Lude.ToPath AssociatePrincipalWithPortfolio where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociatePrincipalWithPortfolio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociatePrincipalWithPortfolioResponse' smart constructor.
newtype AssociatePrincipalWithPortfolioResponse = AssociatePrincipalWithPortfolioResponse'
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

-- | Creates a value of 'AssociatePrincipalWithPortfolioResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociatePrincipalWithPortfolioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociatePrincipalWithPortfolioResponse
mkAssociatePrincipalWithPortfolioResponse pResponseStatus_ =
  AssociatePrincipalWithPortfolioResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apwprsResponseStatus :: Lens.Lens' AssociatePrincipalWithPortfolioResponse Lude.Int
apwprsResponseStatus = Lens.lens (responseStatus :: AssociatePrincipalWithPortfolioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociatePrincipalWithPortfolioResponse)
{-# DEPRECATED apwprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
