{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineSuggester
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures a suggester for a domain. A suggester enables you to display possible matches before users finish typing their queries. When you configure a suggester, you must specify the name of the text field you want to search for possible matches and a unique name for the suggester. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DefineSuggester
  ( -- * Creating a request
    DefineSuggester (..),
    mkDefineSuggester,

    -- ** Request lenses
    defDomainName,
    defSuggester,

    -- * Destructuring the response
    DefineSuggesterResponse (..),
    mkDefineSuggesterResponse,

    -- ** Response lenses
    dsrsResponseStatus,
    dsrsSuggester,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DefineSuggester' @ operation. Specifies the name of the domain you want to update and the suggester configuration.
--
-- /See:/ 'mkDefineSuggester' smart constructor.
data DefineSuggester = DefineSuggester'
  { domainName :: Lude.Text,
    suggester :: Suggester
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefineSuggester' with the minimum fields required to make a request.
--
-- * 'domainName' - Undocumented field.
-- * 'suggester' - Undocumented field.
mkDefineSuggester ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'suggester'
  Suggester ->
  DefineSuggester
mkDefineSuggester pDomainName_ pSuggester_ =
  DefineSuggester'
    { domainName = pDomainName_,
      suggester = pSuggester_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defDomainName :: Lens.Lens' DefineSuggester Lude.Text
defDomainName = Lens.lens (domainName :: DefineSuggester -> Lude.Text) (\s a -> s {domainName = a} :: DefineSuggester)
{-# DEPRECATED defDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'suggester' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defSuggester :: Lens.Lens' DefineSuggester Suggester
defSuggester = Lens.lens (suggester :: DefineSuggester -> Suggester) (\s a -> s {suggester = a} :: DefineSuggester)
{-# DEPRECATED defSuggester "Use generic-lens or generic-optics with 'suggester' instead." #-}

instance Lude.AWSRequest DefineSuggester where
  type Rs DefineSuggester = DefineSuggesterResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DefineSuggesterResult"
      ( \s h x ->
          DefineSuggesterResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "Suggester")
      )

instance Lude.ToHeaders DefineSuggester where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DefineSuggester where
  toPath = Lude.const "/"

instance Lude.ToQuery DefineSuggester where
  toQuery DefineSuggester' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DefineSuggester" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "Suggester" Lude.=: suggester
      ]

-- | The result of a @DefineSuggester@ request. Contains the status of the newly-configured suggester.
--
-- /See:/ 'mkDefineSuggesterResponse' smart constructor.
data DefineSuggesterResponse = DefineSuggesterResponse'
  { responseStatus ::
      Lude.Int,
    suggester :: SuggesterStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefineSuggesterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'suggester' - Undocumented field.
mkDefineSuggesterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'suggester'
  SuggesterStatus ->
  DefineSuggesterResponse
mkDefineSuggesterResponse pResponseStatus_ pSuggester_ =
  DefineSuggesterResponse'
    { responseStatus = pResponseStatus_,
      suggester = pSuggester_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DefineSuggesterResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DefineSuggesterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DefineSuggesterResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'suggester' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSuggester :: Lens.Lens' DefineSuggesterResponse SuggesterStatus
dsrsSuggester = Lens.lens (suggester :: DefineSuggesterResponse -> SuggesterStatus) (\s a -> s {suggester = a} :: DefineSuggesterResponse)
{-# DEPRECATED dsrsSuggester "Use generic-lens or generic-optics with 'suggester' instead." #-}
