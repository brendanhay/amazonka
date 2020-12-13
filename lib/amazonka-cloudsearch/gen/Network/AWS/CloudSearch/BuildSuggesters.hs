{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.BuildSuggesters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indexes the search suggestions. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html#configuring-suggesters Configuring Suggesters> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.BuildSuggesters
  ( -- * Creating a request
    BuildSuggesters (..),
    mkBuildSuggesters,

    -- ** Request lenses
    bsDomainName,

    -- * Destructuring the response
    BuildSuggestersResponse (..),
    mkBuildSuggestersResponse,

    -- ** Response lenses
    bsrsFieldNames,
    bsrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'BuildSuggester' @ operation. Specifies the name of the domain you want to update.
--
-- /See:/ 'mkBuildSuggesters' smart constructor.
newtype BuildSuggesters = BuildSuggesters'
  { domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildSuggesters' with the minimum fields required to make a request.
--
-- * 'domainName' -
mkBuildSuggesters ::
  -- | 'domainName'
  Lude.Text ->
  BuildSuggesters
mkBuildSuggesters pDomainName_ =
  BuildSuggesters' {domainName = pDomainName_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsDomainName :: Lens.Lens' BuildSuggesters Lude.Text
bsDomainName = Lens.lens (domainName :: BuildSuggesters -> Lude.Text) (\s a -> s {domainName = a} :: BuildSuggesters)
{-# DEPRECATED bsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest BuildSuggesters where
  type Rs BuildSuggesters = BuildSuggestersResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "BuildSuggestersResult"
      ( \s h x ->
          BuildSuggestersResponse'
            Lude.<$> ( x Lude..@? "FieldNames" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BuildSuggesters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BuildSuggesters where
  toPath = Lude.const "/"

instance Lude.ToQuery BuildSuggesters where
  toQuery BuildSuggesters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("BuildSuggesters" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @BuildSuggester@ request. Contains a list of the fields used for suggestions.
--
-- /See:/ 'mkBuildSuggestersResponse' smart constructor.
data BuildSuggestersResponse = BuildSuggestersResponse'
  { fieldNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildSuggestersResponse' with the minimum fields required to make a request.
--
-- * 'fieldNames' -
-- * 'responseStatus' - The response status code.
mkBuildSuggestersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BuildSuggestersResponse
mkBuildSuggestersResponse pResponseStatus_ =
  BuildSuggestersResponse'
    { fieldNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'fieldNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrsFieldNames :: Lens.Lens' BuildSuggestersResponse (Lude.Maybe [Lude.Text])
bsrsFieldNames = Lens.lens (fieldNames :: BuildSuggestersResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {fieldNames = a} :: BuildSuggestersResponse)
{-# DEPRECATED bsrsFieldNames "Use generic-lens or generic-optics with 'fieldNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrsResponseStatus :: Lens.Lens' BuildSuggestersResponse Lude.Int
bsrsResponseStatus = Lens.lens (responseStatus :: BuildSuggestersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BuildSuggestersResponse)
{-# DEPRECATED bsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
