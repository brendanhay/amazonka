{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetXSSMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'XssMatchSet' that is specified by @XssMatchSetId@ .
--
--
module Network.AWS.WAF.GetXSSMatchSet
    (
    -- * Creating a Request
      getXSSMatchSet
    , GetXSSMatchSet
    -- * Request Lenses
    , gxmsXSSMatchSetId

    -- * Destructuring the Response
    , getXSSMatchSetResponse
    , GetXSSMatchSetResponse
    -- * Response Lenses
    , gxmsrsXSSMatchSet
    , gxmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | A request to get an 'XssMatchSet' .
--
--
--
-- /See:/ 'getXSSMatchSet' smart constructor.
newtype GetXSSMatchSet = GetXSSMatchSet'
  { _gxmsXSSMatchSetId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetXSSMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gxmsXSSMatchSetId' - The @XssMatchSetId@ of the 'XssMatchSet' that you want to get. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
getXSSMatchSet
    :: Text -- ^ 'gxmsXSSMatchSetId'
    -> GetXSSMatchSet
getXSSMatchSet pXSSMatchSetId_ =
  GetXSSMatchSet' {_gxmsXSSMatchSetId = pXSSMatchSetId_}


-- | The @XssMatchSetId@ of the 'XssMatchSet' that you want to get. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
gxmsXSSMatchSetId :: Lens' GetXSSMatchSet Text
gxmsXSSMatchSetId = lens _gxmsXSSMatchSetId (\ s a -> s{_gxmsXSSMatchSetId = a})

instance AWSRequest GetXSSMatchSet where
        type Rs GetXSSMatchSet = GetXSSMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 GetXSSMatchSetResponse' <$>
                   (x .?> "XssMatchSet") <*> (pure (fromEnum s)))

instance Hashable GetXSSMatchSet where

instance NFData GetXSSMatchSet where

instance ToHeaders GetXSSMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.GetXssMatchSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetXSSMatchSet where
        toJSON GetXSSMatchSet'{..}
          = object
              (catMaybes
                 [Just ("XssMatchSetId" .= _gxmsXSSMatchSetId)])

instance ToPath GetXSSMatchSet where
        toPath = const "/"

instance ToQuery GetXSSMatchSet where
        toQuery = const mempty

-- | The response to a 'GetXssMatchSet' request.
--
--
--
-- /See:/ 'getXSSMatchSetResponse' smart constructor.
data GetXSSMatchSetResponse = GetXSSMatchSetResponse'
  { _gxmsrsXSSMatchSet    :: !(Maybe XSSMatchSet)
  , _gxmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetXSSMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gxmsrsXSSMatchSet' - Information about the 'XssMatchSet' that you specified in the @GetXssMatchSet@ request. For more information, see the following topics:     * 'XssMatchSet' : Contains @Name@ , @XssMatchSetId@ , and an array of @XssMatchTuple@ objects     * 'XssMatchTuple' : Each @XssMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@      * 'FieldToMatch' : Contains @Data@ and @Type@
--
-- * 'gxmsrsResponseStatus' - -- | The response status code.
getXSSMatchSetResponse
    :: Int -- ^ 'gxmsrsResponseStatus'
    -> GetXSSMatchSetResponse
getXSSMatchSetResponse pResponseStatus_ =
  GetXSSMatchSetResponse'
    {_gxmsrsXSSMatchSet = Nothing, _gxmsrsResponseStatus = pResponseStatus_}


-- | Information about the 'XssMatchSet' that you specified in the @GetXssMatchSet@ request. For more information, see the following topics:     * 'XssMatchSet' : Contains @Name@ , @XssMatchSetId@ , and an array of @XssMatchTuple@ objects     * 'XssMatchTuple' : Each @XssMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@      * 'FieldToMatch' : Contains @Data@ and @Type@
gxmsrsXSSMatchSet :: Lens' GetXSSMatchSetResponse (Maybe XSSMatchSet)
gxmsrsXSSMatchSet = lens _gxmsrsXSSMatchSet (\ s a -> s{_gxmsrsXSSMatchSet = a})

-- | -- | The response status code.
gxmsrsResponseStatus :: Lens' GetXSSMatchSetResponse Int
gxmsrsResponseStatus = lens _gxmsrsResponseStatus (\ s a -> s{_gxmsrsResponseStatus = a})

instance NFData GetXSSMatchSetResponse where
