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
-- Module      : Network.AWS.WAFRegional.GetSizeConstraintSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'SizeConstraintSet' specified by @SizeConstraintSetId@ .
--
--
module Network.AWS.WAFRegional.GetSizeConstraintSet
    (
    -- * Creating a Request
      getSizeConstraintSet
    , GetSizeConstraintSet
    -- * Request Lenses
    , gscsSizeConstraintSetId

    -- * Destructuring the Response
    , getSizeConstraintSetResponse
    , GetSizeConstraintSetResponse
    -- * Response Lenses
    , gscsrsSizeConstraintSet
    , gscsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'getSizeConstraintSet' smart constructor.
newtype GetSizeConstraintSet = GetSizeConstraintSet'
  { _gscsSizeConstraintSetId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSizeConstraintSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gscsSizeConstraintSetId' - The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to get. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
getSizeConstraintSet
    :: Text -- ^ 'gscsSizeConstraintSetId'
    -> GetSizeConstraintSet
getSizeConstraintSet pSizeConstraintSetId_ =
  GetSizeConstraintSet' {_gscsSizeConstraintSetId = pSizeConstraintSetId_}


-- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to get. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
gscsSizeConstraintSetId :: Lens' GetSizeConstraintSet Text
gscsSizeConstraintSetId = lens _gscsSizeConstraintSetId (\ s a -> s{_gscsSizeConstraintSetId = a})

instance AWSRequest GetSizeConstraintSet where
        type Rs GetSizeConstraintSet =
             GetSizeConstraintSetResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 GetSizeConstraintSetResponse' <$>
                   (x .?> "SizeConstraintSet") <*> (pure (fromEnum s)))

instance Hashable GetSizeConstraintSet where

instance NFData GetSizeConstraintSet where

instance ToHeaders GetSizeConstraintSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.GetSizeConstraintSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSizeConstraintSet where
        toJSON GetSizeConstraintSet'{..}
          = object
              (catMaybes
                 [Just
                    ("SizeConstraintSetId" .= _gscsSizeConstraintSetId)])

instance ToPath GetSizeConstraintSet where
        toPath = const "/"

instance ToQuery GetSizeConstraintSet where
        toQuery = const mempty

-- | /See:/ 'getSizeConstraintSetResponse' smart constructor.
data GetSizeConstraintSetResponse = GetSizeConstraintSetResponse'
  { _gscsrsSizeConstraintSet :: !(Maybe SizeConstraintSet)
  , _gscsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSizeConstraintSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gscsrsSizeConstraintSet' - Information about the 'SizeConstraintSet' that you specified in the @GetSizeConstraintSet@ request. For more information, see the following topics:     * 'SizeConstraintSet' : Contains @SizeConstraintSetId@ , @SizeConstraints@ , and @Name@      * @SizeConstraints@ : Contains an array of 'SizeConstraint' objects. Each @SizeConstraint@ object contains 'FieldToMatch' , @TextTransformation@ , @ComparisonOperator@ , and @Size@      * 'FieldToMatch' : Contains @Data@ and @Type@
--
-- * 'gscsrsResponseStatus' - -- | The response status code.
getSizeConstraintSetResponse
    :: Int -- ^ 'gscsrsResponseStatus'
    -> GetSizeConstraintSetResponse
getSizeConstraintSetResponse pResponseStatus_ =
  GetSizeConstraintSetResponse'
    { _gscsrsSizeConstraintSet = Nothing
    , _gscsrsResponseStatus = pResponseStatus_
    }


-- | Information about the 'SizeConstraintSet' that you specified in the @GetSizeConstraintSet@ request. For more information, see the following topics:     * 'SizeConstraintSet' : Contains @SizeConstraintSetId@ , @SizeConstraints@ , and @Name@      * @SizeConstraints@ : Contains an array of 'SizeConstraint' objects. Each @SizeConstraint@ object contains 'FieldToMatch' , @TextTransformation@ , @ComparisonOperator@ , and @Size@      * 'FieldToMatch' : Contains @Data@ and @Type@
gscsrsSizeConstraintSet :: Lens' GetSizeConstraintSetResponse (Maybe SizeConstraintSet)
gscsrsSizeConstraintSet = lens _gscsrsSizeConstraintSet (\ s a -> s{_gscsrsSizeConstraintSet = a})

-- | -- | The response status code.
gscsrsResponseStatus :: Lens' GetSizeConstraintSetResponse Int
gscsrsResponseStatus = lens _gscsrsResponseStatus (\ s a -> s{_gscsrsResponseStatus = a})

instance NFData GetSizeConstraintSetResponse where
