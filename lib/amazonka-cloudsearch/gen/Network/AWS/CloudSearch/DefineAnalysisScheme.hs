{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineAnalysisScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an analysis scheme that can be applied to a @text@ or @text-array@ field to define language-specific text processing options. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DefineAnalysisScheme
  ( -- * Creating a request
    DefineAnalysisScheme (..),
    mkDefineAnalysisScheme,

    -- ** Request lenses
    dasaDomainName,
    dasaAnalysisScheme,

    -- * Destructuring the response
    DefineAnalysisSchemeResponse (..),
    mkDefineAnalysisSchemeResponse,

    -- ** Response lenses
    defersResponseStatus,
    defersAnalysisScheme,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DefineAnalysisScheme' @ operation. Specifies the name of the domain you want to update and the analysis scheme configuration.
--
-- /See:/ 'mkDefineAnalysisScheme' smart constructor.
data DefineAnalysisScheme = DefineAnalysisScheme'
  { domainName ::
      Lude.Text,
    analysisScheme :: AnalysisScheme
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefineAnalysisScheme' with the minimum fields required to make a request.
--
-- * 'analysisScheme' - Undocumented field.
-- * 'domainName' - Undocumented field.
mkDefineAnalysisScheme ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'analysisScheme'
  AnalysisScheme ->
  DefineAnalysisScheme
mkDefineAnalysisScheme pDomainName_ pAnalysisScheme_ =
  DefineAnalysisScheme'
    { domainName = pDomainName_,
      analysisScheme = pAnalysisScheme_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasaDomainName :: Lens.Lens' DefineAnalysisScheme Lude.Text
dasaDomainName = Lens.lens (domainName :: DefineAnalysisScheme -> Lude.Text) (\s a -> s {domainName = a} :: DefineAnalysisScheme)
{-# DEPRECATED dasaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasaAnalysisScheme :: Lens.Lens' DefineAnalysisScheme AnalysisScheme
dasaAnalysisScheme = Lens.lens (analysisScheme :: DefineAnalysisScheme -> AnalysisScheme) (\s a -> s {analysisScheme = a} :: DefineAnalysisScheme)
{-# DEPRECATED dasaAnalysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead." #-}

instance Lude.AWSRequest DefineAnalysisScheme where
  type Rs DefineAnalysisScheme = DefineAnalysisSchemeResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DefineAnalysisSchemeResult"
      ( \s h x ->
          DefineAnalysisSchemeResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "AnalysisScheme")
      )

instance Lude.ToHeaders DefineAnalysisScheme where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DefineAnalysisScheme where
  toPath = Lude.const "/"

instance Lude.ToQuery DefineAnalysisScheme where
  toQuery DefineAnalysisScheme' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DefineAnalysisScheme" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "AnalysisScheme" Lude.=: analysisScheme
      ]

-- | The result of a @'DefineAnalysisScheme' @ request. Contains the status of the newly-configured analysis scheme.
--
-- /See:/ 'mkDefineAnalysisSchemeResponse' smart constructor.
data DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse'
  { responseStatus ::
      Lude.Int,
    analysisScheme ::
      AnalysisSchemeStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefineAnalysisSchemeResponse' with the minimum fields required to make a request.
--
-- * 'analysisScheme' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDefineAnalysisSchemeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'analysisScheme'
  AnalysisSchemeStatus ->
  DefineAnalysisSchemeResponse
mkDefineAnalysisSchemeResponse pResponseStatus_ pAnalysisScheme_ =
  DefineAnalysisSchemeResponse'
    { responseStatus = pResponseStatus_,
      analysisScheme = pAnalysisScheme_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defersResponseStatus :: Lens.Lens' DefineAnalysisSchemeResponse Lude.Int
defersResponseStatus = Lens.lens (responseStatus :: DefineAnalysisSchemeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DefineAnalysisSchemeResponse)
{-# DEPRECATED defersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defersAnalysisScheme :: Lens.Lens' DefineAnalysisSchemeResponse AnalysisSchemeStatus
defersAnalysisScheme = Lens.lens (analysisScheme :: DefineAnalysisSchemeResponse -> AnalysisSchemeStatus) (\s a -> s {analysisScheme = a} :: DefineAnalysisSchemeResponse)
{-# DEPRECATED defersAnalysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead." #-}
