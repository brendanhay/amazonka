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
-- Module      : Network.AWS.ELBv2.ModifyTargetGroupAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attributes of the specified target group.
--
--
module Network.AWS.ELBv2.ModifyTargetGroupAttributes
    (
    -- * Creating a Request
      modifyTargetGroupAttributes
    , ModifyTargetGroupAttributes
    -- * Request Lenses
    , mtgaTargetGroupARN
    , mtgaAttributes

    -- * Destructuring the Response
    , modifyTargetGroupAttributesResponse
    , ModifyTargetGroupAttributesResponse
    -- * Response Lenses
    , mtgarsAttributes
    , mtgarsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyTargetGroupAttributes' smart constructor.
data ModifyTargetGroupAttributes = ModifyTargetGroupAttributes'
  { _mtgaTargetGroupARN :: !Text
  , _mtgaAttributes     :: ![TargetGroupAttribute]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyTargetGroupAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgaTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
--
-- * 'mtgaAttributes' - The attributes.
modifyTargetGroupAttributes
    :: Text -- ^ 'mtgaTargetGroupARN'
    -> ModifyTargetGroupAttributes
modifyTargetGroupAttributes pTargetGroupARN_ =
  ModifyTargetGroupAttributes'
    {_mtgaTargetGroupARN = pTargetGroupARN_, _mtgaAttributes = mempty}


-- | The Amazon Resource Name (ARN) of the target group.
mtgaTargetGroupARN :: Lens' ModifyTargetGroupAttributes Text
mtgaTargetGroupARN = lens _mtgaTargetGroupARN (\ s a -> s{_mtgaTargetGroupARN = a})

-- | The attributes.
mtgaAttributes :: Lens' ModifyTargetGroupAttributes [TargetGroupAttribute]
mtgaAttributes = lens _mtgaAttributes (\ s a -> s{_mtgaAttributes = a}) . _Coerce

instance AWSRequest ModifyTargetGroupAttributes where
        type Rs ModifyTargetGroupAttributes =
             ModifyTargetGroupAttributesResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper
              "ModifyTargetGroupAttributesResult"
              (\ s h x ->
                 ModifyTargetGroupAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ModifyTargetGroupAttributes where

instance NFData ModifyTargetGroupAttributes where

instance ToHeaders ModifyTargetGroupAttributes where
        toHeaders = const mempty

instance ToPath ModifyTargetGroupAttributes where
        toPath = const "/"

instance ToQuery ModifyTargetGroupAttributes where
        toQuery ModifyTargetGroupAttributes'{..}
          = mconcat
              ["Action" =:
                 ("ModifyTargetGroupAttributes" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "TargetGroupArn" =: _mtgaTargetGroupARN,
               "Attributes" =: toQueryList "member" _mtgaAttributes]

-- | /See:/ 'modifyTargetGroupAttributesResponse' smart constructor.
data ModifyTargetGroupAttributesResponse = ModifyTargetGroupAttributesResponse'
  { _mtgarsAttributes     :: !(Maybe [TargetGroupAttribute])
  , _mtgarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyTargetGroupAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgarsAttributes' - Information about the attributes.
--
-- * 'mtgarsResponseStatus' - -- | The response status code.
modifyTargetGroupAttributesResponse
    :: Int -- ^ 'mtgarsResponseStatus'
    -> ModifyTargetGroupAttributesResponse
modifyTargetGroupAttributesResponse pResponseStatus_ =
  ModifyTargetGroupAttributesResponse'
    {_mtgarsAttributes = Nothing, _mtgarsResponseStatus = pResponseStatus_}


-- | Information about the attributes.
mtgarsAttributes :: Lens' ModifyTargetGroupAttributesResponse [TargetGroupAttribute]
mtgarsAttributes = lens _mtgarsAttributes (\ s a -> s{_mtgarsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
mtgarsResponseStatus :: Lens' ModifyTargetGroupAttributesResponse Int
mtgarsResponseStatus = lens _mtgarsResponseStatus (\ s a -> s{_mtgarsResponseStatus = a})

instance NFData ModifyTargetGroupAttributesResponse
         where
